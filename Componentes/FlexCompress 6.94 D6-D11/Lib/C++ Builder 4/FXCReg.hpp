// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCReg.pas' rev: 4.00

#ifndef FXCRegHPP
#define FXCRegHPP

#pragma delphiheader begin
#pragma option push -w-
#include <DsgnIntf.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcreg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFXCArchiveFileName;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TFXCArchiveFileName : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TFXCArchiveFileName(const Dsgnintf::_di_IFormDesigner 
		ADesigner, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCArchiveFileName(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TFXCSFXFileName;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TFXCSFXFileName : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TFXCSFXFileName(const Dsgnintf::_di_IFormDesigner ADesigner
		, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCSFXFileName(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TFXCDirectory;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TFXCDirectory : public Dsgnintf::TStringProperty 
{
	typedef Dsgnintf::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall TFXCDirectory(const Dsgnintf::_di_IFormDesigner ADesigner
		, int APropCount) : Dsgnintf::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFXCDirectory(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Fxcreg */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcreg;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCReg
