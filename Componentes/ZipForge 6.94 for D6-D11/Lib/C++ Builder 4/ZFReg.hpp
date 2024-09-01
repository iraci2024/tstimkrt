// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFReg.pas' rev: 4.00

#ifndef ZFRegHPP
#define ZFRegHPP

#pragma delphiheader begin
#pragma option push -w-
#include <DsgnIntf.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfreg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TZFArchiveFileName;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TZFArchiveFileName : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TZFArchiveFileName(const Dsgnintf::_di_IFormDesigner 
		ADesigner, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TZFArchiveFileName(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TZFSFXFileName;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TZFSFXFileName : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TZFSFXFileName(const Dsgnintf::_di_IFormDesigner ADesigner
		, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TZFSFXFileName(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TZFDirectory;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TZFDirectory : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TZFDirectory(const Dsgnintf::_di_IFormDesigner ADesigner
		, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TZFDirectory(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Zfreg */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfreg;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFReg
