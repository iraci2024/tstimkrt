// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCResponsiveManagerDimensionsEditor.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncresponsivemanagerdimensionseditorHPP
#define Vcl_TmsfncresponsivemanagerdimensionseditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCResponsiveManager.hpp>
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
namespace Tmsfncresponsivemanagerdimensionseditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorDimension;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorDimensions;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditor;
class DELPHICLASS TTMSFNCResponsiveManagerDimensionsEditorForm;
//-- type declarations -------------------------------------------------------
typedef Vcl::Controls::TWinControl TTMSFNCResponsiveManagerDimensionsEditorParent;

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
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>() { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(System::Generics::Collections::TEnumerable__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditorDimensions() { }
	
public:
	/* {System_Generics_Collections}TList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(System::Generics::Collections::TEnumerable__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCResponsiveManagerDimensionsEditor_TTMSFNCResponsiveManagerDimensionsEditorDimension>.Create */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorDimensions(TTMSFNCResponsiveManagerDimensionsEditorDimension* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCResponsiveManagerDimensionsEditorDimension*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditor : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	TTMSFNCResponsiveManagerDimensionsEditorDimensions* FDimensions;
	Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonNew;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAdd;
	Vcl::Stdctrls::TLabel* FLblTitle;
	Vcl::Stdctrls::TLabel* FLblWidth;
	Vcl::Stdctrls::TLabel* FLblHeight;
	Vcl::Stdctrls::TLabel* FLblType;
	Vcl::Stdctrls::TEdit* FEdTitle;
	Vcl::Stdctrls::TEdit* FEdWidth;
	Vcl::Stdctrls::TEdit* FEdHeight;
	Vcl::Stdctrls::TComboBox* FCbType;
	Vcl::Tmsfnceditorlistview::TTMSFNCEditorList* FListBox;
	Vcl::Forms::TScrollBox* FVScrlBox;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall LoadDimensions();
	void __fastcall DoListClick(System::TObject* Sender, int AIndex, Vcl::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoDelete(System::TObject* Sender);
	void __fastcall DoAdd(System::TObject* Sender);
	void __fastcall DoNew(System::TObject* Sender);
	void __fastcall DoFormResize(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall BuildEditor(Vcl::Controls::TWinControl* AParent);
	void __fastcall DoListBoxResize(System::TObject* Sender);
	void __fastcall RealignControls();
	
public:
	__fastcall virtual TTMSFNCResponsiveManagerDimensionsEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditor(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerDimensionsEditorForm : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCResponsiveManagerDimensionsEditorForm(System::Classes::TComponent* AOwner) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCResponsiveManagerDimensionsEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerDimensionsEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCResponsiveManagerDimensionsEditorForm(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorOK;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorOK System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorCancel;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorCancel System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorDelete;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorDelete System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorDelete)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorAdd;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorAdd System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorAdd)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorUpdate;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorUpdate System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorUpdate)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorNew;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorNew System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorNew)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorTitle;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorTitle System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorTitle)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorWidth;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorWidth System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorWidth)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorHeight;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorHeight System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorHeight)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCResponsiveManagerDimensionsEditorType;
#define Vcl_Tmsfncresponsivemanagerdimensionseditor_sTMSFNCResponsiveManagerDimensionsEditorType System::LoadResourceString(&Vcl::Tmsfncresponsivemanagerdimensionseditor::_sTMSFNCResponsiveManagerDimensionsEditorType)
}	/* namespace Tmsfncresponsivemanagerdimensionseditor */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCRESPONSIVEMANAGERDIMENSIONSEDITOR)
using namespace Vcl::Tmsfncresponsivemanagerdimensionseditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncresponsivemanagerdimensionseditorHPP
