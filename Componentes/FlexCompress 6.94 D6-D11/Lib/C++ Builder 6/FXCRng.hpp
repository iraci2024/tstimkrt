// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCRNG.pas' rev: 6.00

#ifndef FXCRNGHPP
#define FXCRNGHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <FXCDecUtil.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcrng
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ERandom;
class PASCALIMPLEMENTATION ERandom : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ERandom(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ERandom(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ERandom(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ERandom(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ERandom(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ERandom(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ERandom(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ERandom(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ERandom(void) { }
	#pragma option pop
	
};


class DELPHICLASS TRandom;
class PASCALIMPLEMENTATION TRandom : public Fxcdecutil::TProtection 
{
	typedef Fxcdecutil::TProtection inherited;
	
private:
	int FRegister;
	AnsiString FPassword;
	
protected:
	int FCount;
	int FSize;
	int FBasicSeed;
	virtual void __fastcall SetSize(int Value);
	virtual AnsiString __fastcall GetState();
	virtual void __fastcall SetState(AnsiString Value);
	virtual void __fastcall CodeInit(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Fxcdecutil::TPAction Action);
	
public:
	__fastcall virtual TRandom(const AnsiString APassword, int ASize, bool ARandomize, Fxcdecutil::TProtection* AProtection);
	__fastcall virtual ~TRandom(void);
	virtual void __fastcall Seed(const void *ABuffer, int ASize);
	virtual void __fastcall Buffer(void *ABuffer, int ASize);
	virtual int __fastcall Int(int ARange);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	void __fastcall SaveToFile(const AnsiString FileName);
	void __fastcall LoadFromFile(const AnsiString FileName);
	__property int Count = {read=FCount, write=FCount, nodefault};
	__property int Size = {read=FSize, write=SetSize, nodefault};
	__property int BasicSeed = {read=FBasicSeed, write=FBasicSeed, nodefault};
	__property AnsiString State = {read=GetState, write=SetState};
};


class DELPHICLASS TRandom_LFSR;
class PASCALIMPLEMENTATION TRandom_LFSR : public TRandom 
{
	typedef TRandom inherited;
	
private:
	int FPtr;
	int FLast;
	Word FTable[256];
	Byte FRegister[256];
	void __fastcall (*FFunc)(void * Self, void *Buffer, int Size);
	
protected:
	virtual void __fastcall SetSize(int Value);
	virtual AnsiString __fastcall GetState();
	virtual void __fastcall SetState(AnsiString Value);
	
public:
	virtual void __fastcall Seed(const void *ABuffer, int ASize);
	virtual void __fastcall Buffer(void *ABuffer, int ASize);
public:
	#pragma option push -w-inl
	/* TRandom.Create */ inline __fastcall virtual TRandom_LFSR(const AnsiString APassword, int ASize, bool ARandomize, Fxcdecutil::TProtection* AProtection) : TRandom(APassword, ASize, ARandomize, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TRandom.Destroy */ inline __fastcall virtual ~TRandom_LFSR(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int DefaultSeed;
extern PACKAGE TRandom* __fastcall RND(void);

}	/* namespace Fxcrng */
using namespace Fxcrng;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCRNG
