// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCResponsiveManagerDimensionsEditor.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncresponsivemanagerdimensionseditorHPP
#define Fmx_TmsfncresponsivemanagerdimensionseditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCResponsiveManager.hpp>
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.TMSFNCEditorListView.hpp>
#include <FMX.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Edit.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ComboEdit.hpp>
#include <FMX.Layouts.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncresponsivemanagerdimensionseditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorDimension;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorDimensions;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditor;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorForm;
//-- type declarations -------------------------------------------------------
typedef Fmx::Types::TFmxObject TTMSFNCResponsiveManagerDimensionsEditorParent;

enum DECLSPEC_DENUM TTMSFNCResponsiveManagerDimensionsEditorType : unsigned char { edtDesktop, edtPhone, edtTablet };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditorDimension : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FWidth;
	System::UnicodeString FTitle;
	int FHeight;
	bool FDefault;
	TTMSFNCResponsiveManagerDimensionsEditorType FType;
	bool FResource;
	
public:
	__property bool Resource = {read=FResource, write=FResource, nodefault};
	__fastcall virtual TTMSFNCResponsiveManagerDimensionsEditorDimension();
	
__published:
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property int Width = {read=FWidth, write=FWidth, nodefault};
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	__property bool Default = {read=FDefault, write=FDefault, default=1};
	__property TTMSFNCResponsiveManagerDimensionsEditorType Type = {read=FType, write=FType, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditorDimension() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditorDimensions : public System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(System::Generics::Collections::TEnumerable__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditorDimensions() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(System::Generics::Collections::TEnumerable__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(TTMSFNCResponsiveManagerDimensionsEditorDimension* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	TTMSFNCResponsiveManagerDimensionsEditorDimensions* FDimensions;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonNew;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAdd;
	Fmx::Stdctrls::TLabel* FLblTitle;
	Fmx::Stdctrls::TLabel* FLblWidth;
	Fmx::Stdctrls::TLabel* FLblHeight;
	Fmx::Stdctrls::TLabel* FLblType;
	Fmx::Edit::TEdit* FEdTitle;
	Fmx::Edit::TEdit* FEdWidth;
	Fmx::Edit::TEdit* FEdHeight;
	Fmx::Listbox::TComboBox* FCbType;
	Fmx::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall LoadDimensions();
	void __fastcall DoListClick(System::TObject* Sender, int AIndex, Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoDelete(System::TObject* Sender);
	void __fastcall DoAdd(System::TObject* Sender);
	void __fastcall DoNew(System::TObject* Sender);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	void __fastcall DoListBoxResize(System::TObject* Sender);
	void __fastcall RealignControls();
	
public:
	__fastcall virtual TTMSFNCResponsiveManagerDimensionsEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCResponsiveManagerDimensionsEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCResponsiveManagerDimensionsEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditorForm() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorOK;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorOK System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorCancel;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorCancel System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorDelete;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorDelete System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorDelete)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorAdd;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorAdd System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorAdd)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorUpdate;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorUpdate System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorUpdate)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorNew;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorNew System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorNew)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorTitle;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorTitle System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorTitle)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorWidth;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorWidth System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorWidth)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorHeight;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorHeight System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorHeight)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorType;
#define Fmx_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorType System::LoadResourceString(&Fmx::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorType)
}	/* namespace Tmsfncresponsivemanagerdimensionseditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCRESPONSIVEMANAGERDIMENSIONSEDITOR)
using namespace Fmx::Tmsfncresponsivemanagerdimensionseditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncresponsivemanagerdimensionseditorHPP
