// CodeGear C++Builder
// Copyright (c) 1995, 2011 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCReg.pas' rev: 23.00 (Win32)

#ifndef FxcregHPP
#define FxcregHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcreg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFXCArchiveFileName;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFXCArchiveFileName : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TFXCArchiveFileName(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCArchiveFileName(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFXCSFXFileName;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFXCSFXFileName : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TFXCSFXFileName(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCSFXFileName(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFXCDirectory;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFXCDirectory : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TFXCDirectory(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCDirectory(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Fxcreg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCREG)
using namespace Fxcreg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcregHPP
