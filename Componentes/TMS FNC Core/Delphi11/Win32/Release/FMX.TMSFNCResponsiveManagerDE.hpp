// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCResponsiveManagerDE.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncresponsivemanagerdeHPP
#define Fmx_TmsfncresponsivemanagerdeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCResponsiveManager.hpp>
#include <FMX.TMSFNCGeneralDE.hpp>
#include <FMX.TMSFNCResponsiveManagerDimensionsEditor.hpp>
#include <System.TypInfo.hpp>
#include <DesignEditors.hpp>
#include <DesignIntf.hpp>
#include <DesignMenus.hpp>
#include <Vcl.Menus.hpp>
#include <System.Generics.Collections.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncresponsivemanagerde
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCResponsiveManagerEditor;
class DELPHICLASS TTMSFNCResponsiveManagerStateProperty;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerEditor : public Fmx::Tmsfncgeneralde::TTMSFNCDefaultEditor
{
	typedef Fmx::Tmsfncgeneralde::TTMSFNCDefaultEditor inherited;
	
private:
	Fmx::Tmsfncresponsivemanagerdimensionseditor::TTMSFNCResponsiveManagerDimensionsEditorDimensions* FDimensions;
	Fmx::Tmsfncresponsivemanagerdimensionseditor::TTMSFNCResponsiveManagerDimensionsEditorDimensions* __fastcall GetDimensions();
	
protected:
	virtual void __fastcall LoadDimensions();
	__property Fmx::Tmsfncresponsivemanagerdimensionseditor::TTMSFNCResponsiveManagerDimensionsEditorDimensions* Dimensions = {read=GetDimensions};
	
public:
	__fastcall virtual ~TTMSFNCResponsiveManagerEditor();
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
	void __fastcall AddSaveSubMenuItem(int ATag, System::UnicodeString AText, bool AChecked, const Designmenus::_di_IMenuItem AItem);
	void __fastcall AddLoadSubMenuItem(int ATag, System::UnicodeString AText, bool AChecked, const Designmenus::_di_IMenuItem AItem);
	void __fastcall AddDimensionSubMenuItem(int ATag, System::UnicodeString AText, const Designmenus::_di_IMenuItem AItem);
	void __fastcall AddSelectSubMenuItem(int ATag, System::UnicodeString AText, bool AChecked, const Designmenus::_di_IMenuItem AItem);
	void __fastcall MenuItemSaveExecute(System::TObject* Sender);
	void __fastcall MenuItemLoadExecute(System::TObject* Sender);
	void __fastcall MenuItemDimensionExecute(System::TObject* Sender);
	void __fastcall MenuItemSelectExecute(System::TObject* Sender);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty PropertyEditor, bool &Continue);
	void __fastcall OpenSettings(Fmx::Tmsfncresponsivemanager::TTMSFNCResponsiveManager* AManager);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TTMSFNCResponsiveManagerEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Fmx::Tmsfncgeneralde::TTMSFNCDefaultEditor(AComponent, ADesigner) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCResponsiveManagerStateProperty : public Designeditors::TIntegerProperty
{
	typedef Designeditors::TIntegerProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCResponsiveManagerStateProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TIntegerProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCResponsiveManagerStateProperty() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncresponsivemanagerde */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCRESPONSIVEMANAGERDE)
using namespace Fmx::Tmsfncresponsivemanagerde;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncresponsivemanagerdeHPP
