// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCDataBindingDE.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncdatabindingdeHPP
#define Vcl_TmsfncdatabindingdeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCDataBinding.hpp>
#include <VCL.TMSFNCGeneralDE.hpp>
#include <VCL.TMSFNCDataBindingEditor.hpp>
#include <System.TypInfo.hpp>
#include <DesignEditors.hpp>
#include <DesignIntf.hpp>
#include <DesignMenus.hpp>
#include <Vcl.Menus.hpp>
#include <Data.DB.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncdatabindingde
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCDataBinderEditor;
class DELPHICLASS TTMSFNCDataBinderObjectProperty;
class DELPHICLASS TTMSFNCDataBinderColumnsProperty;
class DELPHICLASS TTMSFNCDataBinderColumnsSubProperty;
class DELPHICLASS TTMSFNCDataBinderFieldNameProperty;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBinderEditor : public Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor
{
	typedef Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor inherited;
	
public:
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
	HIDESBASE void __fastcall MenuItemExecute(System::TObject* Sender);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty PropertyEditor, bool &Continue);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TTMSFNCDataBinderEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBinderObjectProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCDataBinderObjectProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderObjectProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBinderColumnsProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCDataBinderColumnsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderColumnsProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBinderColumnsSubProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCDataBinderColumnsSubProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderColumnsSubProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCDataBinderFieldNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCDataBinderFieldNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCDataBinderFieldNameProperty() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncdatabindingde */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCDATABINDINGDE)
using namespace Vcl::Tmsfncdatabindingde;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncdatabindingdeHPP
