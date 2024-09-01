// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCStylesEditor.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncstyleseditorHPP
#define Vcl_TmsfncstyleseditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCStyles.hpp>
#include <VCL.TMSFNCEditorButton.hpp>
#include <VCL.TMSFNCEditorPanel.hpp>
#include <VCL.TMSFNCEditorListView.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncstyleseditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCStylesEditorComboBox;
class DELPHICLASS TTMSFNCStylesListItem;
class DELPHICLASS TTMSFNCStylesListItems;
class DELPHICLASS TTMSFNCStylesListItemsRef;
class DELPHICLASS TTMSFNCStylesVisualizer;
class DELPHICLASS TTMSFNCStylesEditor;
class DELPHICLASS TTMSFNCStylesEditorForm;
//-- type declarations -------------------------------------------------------
typedef Vcl::Controls::TWinControl TTMSFNCStylesEditorParent;

class PASCALIMPLEMENTATION TTMSFNCStylesEditorComboBox : public Vcl::Stdctrls::TComboBox
{
	typedef Vcl::Stdctrls::TComboBox inherited;
	
public:
	/* TCustomComboBox.Create */ inline __fastcall virtual TTMSFNCStylesEditorComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~TTMSFNCStylesEditorComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCStylesEditorComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesListItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FResource;
	System::UnicodeString FName;
	System::UnicodeString FDisplayName;
	
public:
	__fastcall TTMSFNCStylesListItem(bool AResource, System::UnicodeString AName, System::UnicodeString ADisplayName);
	__property bool Resource = {read=FResource, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString DisplayName = {read=FDisplayName};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItem() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesListItems : public System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>() { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItems() { }
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(TTMSFNCStylesListItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesListItemsRef : public System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCStylesListItem*> inherited;
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>() { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(TTMSFNCStylesListItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItemsRef() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesVisualizer : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	TTMSFNCStylesListItemsRef* FItems;
	
public:
	__fastcall virtual TTMSFNCStylesVisualizer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCStylesVisualizer();
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	__property TTMSFNCStylesListItemsRef* Items = {read=FItems, write=FItems};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCStylesVisualizer(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	TTMSFNCStylesListItems* FItems;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FSidePanel;
	TTMSFNCStylesVisualizer* FVisualizer;
	Vcl::Stdctrls::TLabel* FSelectedStyle;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCombine;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonApply;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOpen;
	Vcl::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Vcl::Forms::TScrollBox* FVScrlBox;
	Vcl::Extctrls::TSplitter* FSplitter;
	Vcl::Tmsfncstyles::TTMSFNCStylesManager* FStylesManager;
	Vcl::Forms::TCustomForm* FStylesForm;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoListClick(System::TObject* Sender, int AIndex, Vcl::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoApply(System::TObject* Sender);
	void __fastcall DoCombine(System::TObject* Sender);
	void __fastcall DoDelete(System::TObject* Sender);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
	virtual void __fastcall DoOpenFile(System::TObject* Sender);
	void __fastcall DoListBoxResize(System::TObject* Sender);
	void __fastcall RealignControls();
	
public:
	__fastcall virtual TTMSFNCStylesEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCStylesEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	__property Vcl::Forms::TCustomForm* StylesForm = {read=FStylesForm, write=FStylesForm};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod void __fastcall LoadStylesFromResources(System::Classes::TStrings* AStrings);
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCStylesEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCStylesEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCStylesEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCStylesEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCStylesEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCStylesEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x1);
static const System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorOK;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorOK System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorCancel;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorCancel System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorOpen;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorOpen System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorOpen)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorApply;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorApply System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorApply)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorCombine;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorCombine System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorCombine)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorDelete;
#define Vcl_Tmsfncstyleseditor_sTMSFNCStylesEditorDelete System::LoadResourceString(&Vcl::Tmsfncstyleseditor::_sTMSFNCStylesEditorDelete)
}	/* namespace Tmsfncstyleseditor */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCSTYLESEDITOR)
using namespace Vcl::Tmsfncstyleseditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncstyleseditorHPP
