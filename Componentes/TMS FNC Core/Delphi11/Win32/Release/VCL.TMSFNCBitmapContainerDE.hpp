// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCBitmapContainerDE.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncbitmapcontainerdeHPP
#define Vcl_TmsfncbitmapcontainerdeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCGeneralDE.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>
#include <DesignEditors.hpp>
#include <DesignIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncbitmapcontainerde
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCBitmapContainerDEEditor;
class DELPHICLASS TTMSFNCBitmapNameProperty;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapContainerDEEditor : public Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor
{
	typedef Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor inherited;
	
public:
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
	virtual void __fastcall ExecuteVerb(int Index);
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty PropertyEditor, bool &Continue);
	void __fastcall FillItems(Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* ABitmapContainer);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TTMSFNCBitmapContainerDEEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Vcl::Tmsfncgeneralde::TTMSFNCDefaultEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapContainerDEEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCBitmapNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TTMSFNCBitmapNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapNameProperty() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncbitmapcontainerde */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCBITMAPCONTAINERDE)
using namespace Vcl::Tmsfncbitmapcontainerde;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncbitmapcontainerdeHPP
