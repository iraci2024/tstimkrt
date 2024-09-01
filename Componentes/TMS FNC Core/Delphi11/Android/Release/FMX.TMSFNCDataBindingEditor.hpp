// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCDataBindingEditor.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncdatabindingeditorHPP
#define Fmx_TmsfncdatabindingeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCDataBinding.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.TMSFNCEditorListView.hpp>
#include <FMX.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ComboEdit.hpp>
#include <FMX.Layouts.hpp>
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
namespace Tmsfncdatabindingeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCDataBindingEditorComboBox;
class DELPHICLASS TTMSFNCDataBindingEditorComboBoxSub;
struct TTMSFNCDataBindingEditorBuildItem;
class DELPHICLASS TTMSFNCDataBindingEditor;
class DELPHICLASS TTMSFNCDataBindingEditorForm;
//-- type declarations -------------------------------------------------------
typedef Fmx::Types::TFmxObject TTMSFNCDataBindingEditorParent;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorComboBox : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorComboBox(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorComboBox() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorComboBoxSub : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorComboBoxSub(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorComboBoxSub() { }
	
};

#pragma pack(pop)

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
	Fmx::Tmsfncdatabinding::TTMSFNCDataBinderItem* Item;
	TTMSFNCDataBindingTRectFArray SubPropertyNamesRects;
	TTMSFNCDataBindingTRectFArray SubFieldNamesRects;
	TTMSFNCDataBindingStringArray SubPropertyNamesTexts;
	TTMSFNCDataBindingStringArray SubFieldNamesTexts;
	TTMSFNCDataBindingTComboArray SubPropertyNamesCombos;
	TTMSFNCDataBindingTComboArray SubFieldNamesCombos;
};


enum DECLSPEC_DENUM TTMSFNCDataBindingEditorMode : unsigned char { dbemView, dbemEdit };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBindingEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FCachedComponentScreenShot;
	bool FBlockChange;
	TTMSFNCDataBindingEditorMode FMode;
	TTMSFNCDataBindingEditorBuildItem FBuildItem;
	Fmx::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonEdit;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAdd;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl* FPreview;
	Fmx::Tmsfncdatabinding::TTMSFNCDataBinder* FDataBinder;
	Fmx::Stdctrls::TCheckBox* FCheckBoxActive;
	Fmx::Listbox::TComboBox* FComboBoxBindType;
	Fmx::Listbox::TComboBox* FComboboxBindComponent;
	TTMSFNCDataBindingEditorComboBox* FComboBoxFieldName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxPropertyName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxColumnsPropertyName;
	TTMSFNCDataBindingEditorComboBox* FComboBoxColumnsSubPropertyName;
	Fmx::Listbox::TComboBox* FComboBoxDataSource;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FSubAdd;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FSubDelete;
	System::TObject* FObject;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual void __fastcall RegisterRuntimeClasses();
	void __fastcall MakeComponentScreenShot();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
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
	void __fastcall DoListBoxItemSelectedChange(System::TObject* Sender, int AIndex, Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoBindComponentChanged(System::TObject* Sender);
	void __fastcall DoPropertyNameChanged(System::TObject* Sender);
	void __fastcall DoFieldNameChanged(System::TObject* Sender);
	void __fastcall DoComboSubPropertyChanged(System::TObject* Sender);
	void __fastcall DoComboSubFieldChanged(System::TObject* Sender);
	void __fastcall DoColumnsPropertyNameChanged(System::TObject* Sender);
	void __fastcall DoColumnsSubPropertyNameChanged(System::TObject* Sender);
	virtual void __fastcall DoAfterDraw(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	
public:
	__fastcall virtual TTMSFNCDataBindingEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCDataBindingEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncdatabinding::TTMSFNCDataBinder* DataBinder = {read=FDataBinder, write=FDataBinder};
	__property System::TObject* Object = {read=FObject, write=FObject};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBindingEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCDataBindingEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCDataBindingEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCDataBindingEditorForm() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorOK;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorOK System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorCancel;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorCancel System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorEditMode;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorEditMode System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorEditMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorCloseEditMode;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorCloseEditMode System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorCloseEditMode)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorActive;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorActive System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorActive)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorAdd;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorAdd System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorAdd)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCDataBindingEditorDelete;
#define Fmx_Tmsfncdatabindingeditor_sTMSFNCDataBindingEditorDelete System::LoadResourceString(&Fmx::Tmsfncdatabindingeditor::_sTMSFNCDataBindingEditorDelete)
}	/* namespace Tmsfncdatabindingeditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCDATABINDINGEDITOR)
using namespace Fmx::Tmsfncdatabindingeditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncdatabindingeditorHPP
