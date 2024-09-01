// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCStylesEditor.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncstyleseditorHPP
#define Fmx_TmsfncstyleseditorHPP

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
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCStyles.hpp>
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
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Controls.Presentation.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
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
typedef Fmx::Types::TFmxObject TTMSFNCStylesEditorParent;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesEditorComboBox : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCStylesEditorComboBox(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCStylesEditorComboBox() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
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
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesListItems : public System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItems() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItems(TTMSFNCStylesListItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCStylesListItem*>(Values, Values_High) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesListItemsRef : public System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCStylesListItem*> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCStylesListItem*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(System::Generics::Collections::TEnumerable__1<TTMSFNCStylesListItem*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Create */ inline __fastcall TTMSFNCStylesListItemsRef(TTMSFNCStylesListItem* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCStylesListItem*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCStylesEditor_TTMSFNCStylesListItem>.Destroy */ inline __fastcall virtual ~TTMSFNCStylesListItemsRef() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesVisualizer : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	TTMSFNCStylesListItemsRef* FItems;
	
public:
	__fastcall virtual TTMSFNCStylesVisualizer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCStylesVisualizer();
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	__property TTMSFNCStylesListItemsRef* Items = {read=FItems, write=FItems};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	TTMSFNCStylesListItems* FItems;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FSidePanel;
	TTMSFNCStylesVisualizer* FVisualizer;
	Fmx::Stdctrls::TLabel* FSelectedStyle;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCombine;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonApply;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOpen;
	Fmx::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	Fmx::Stdctrls::TSplitter* FSplitter;
	Fmx::Tmsfncstyles::TTMSFNCStylesManager* FStylesManager;
	Fmx::Forms::TCustomForm* FStylesForm;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall DoListClick(System::TObject* Sender, int AIndex, Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoApply(System::TObject* Sender);
	void __fastcall DoCombine(System::TObject* Sender);
	void __fastcall DoDelete(System::TObject* Sender);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	virtual void __fastcall DoOpenFile(System::TObject* Sender);
	void __fastcall DoListBoxResize(System::TObject* Sender);
	void __fastcall RealignControls();
	
public:
	__fastcall virtual TTMSFNCStylesEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCStylesEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	__property Fmx::Forms::TCustomForm* StylesForm = {read=FStylesForm, write=FStylesForm};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod void __fastcall LoadStylesFromResources(System::Classes::TStrings* AStrings);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCStylesEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCStylesEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCStylesEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCStylesEditorForm() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x1);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorOK;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorOK System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorCancel;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorCancel System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorOpen;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorOpen System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorOpen)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorApply;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorApply System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorApply)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorCombine;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorCombine System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorCombine)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCStylesEditorDelete;
#define Fmx_Tmsfncstyleseditor_sTMSFNCStylesEditorDelete System::LoadResourceString(&Fmx::Tmsfncstyleseditor::_sTMSFNCStylesEditorDelete)
}	/* namespace Tmsfncstyleseditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCSTYLESEDITOR)
using namespace Fmx::Tmsfncstyleseditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncstyleseditorHPP
