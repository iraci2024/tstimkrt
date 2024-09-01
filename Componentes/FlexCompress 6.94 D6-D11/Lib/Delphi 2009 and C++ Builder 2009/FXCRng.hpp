// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcrng.pas' rev: 20.00

#ifndef FxcrngHPP
#define FxcrngHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Fxcdecutil.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcrng
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ERandom;
class PASCALIMPLEMENTATION ERandom : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ERandom(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ERandom(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ERandom(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall ERandom(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ERandom(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ERandom(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERandom(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERandom(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ERandom(void) { }
	
};


class DELPHICLASS TRandom;
class PASCALIMPLEMENTATION TRandom : public Fxcdecutil::TProtection
{
	typedef Fxcdecutil::TProtection inherited;
	
private:
	int FRegister;
	System::AnsiString FPassword;
	
protected:
	int FCount;
	int FSize;
	int FBasicSeed;
	virtual void __fastcall SetSize(int Value);
	virtual System::AnsiString __fastcall GetState();
	virtual void __fastcall SetState(System::AnsiString Value);
	virtual void __fastcall CodeInit(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Fxcdecutil::TPAction Action);
	
public:
	__fastcall virtual TRandom(const System::AnsiString APassword, int ASize, bool ARandomize, Fxcdecutil::TProtection* AProtection);
	__fastcall virtual ~TRandom(void);
	virtual void __fastcall Seed(const void *ABuffer, int ASize);
	virtual void __fastcall Buffer(void *ABuffer, int ASize);
	virtual int __fastcall Int(int ARange);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	void __fastcall SaveToFile(const System::AnsiString FileName);
	void __fastcall LoadFromFile(const System::AnsiString FileName);
	__property int Count = {read=FCount, write=FCount, nodefault};
	__property int Size = {read=FSize, write=SetSize, nodefault};
	__property int BasicSeed = {read=FBasicSeed, write=FBasicSeed, nodefault};
	__property System::AnsiString State = {read=GetState, write=SetState};
};


class DELPHICLASS TRandom_LFSR;
class PASCALIMPLEMENTATION TRandom_LFSR : public TRandom
{
	typedef TRandom inherited;
	
private:
	int FPtr;
	int FLast;
	StaticArray<System::Word, 256> FTable;
	StaticArray<System::Byte, 256> FRegister;
	void __fastcall (*FFunc)(void * Self, void *Buffer, int Size);
	
protected:
	virtual void __fastcall SetSize(int Value);
	virtual System::AnsiString __fastcall GetState();
	virtual void __fastcall SetState(System::AnsiString Value);
	
public:
	virtual void __fastcall Seed(const void *ABuffer, int ASize);
	virtual void __fastcall Buffer(void *ABuffer, int ASize);
public:
	/* TRandom.Create */ inline __fastcall virtual TRandom_LFSR(const System::AnsiString APassword, int ASize, bool ARandomize, Fxcdecutil::TProtection* AProtection) : TRandom(APassword, ASize, ARandomize, AProtection) { }
	/* TRandom.Destroy */ inline __fastcall virtual ~TRandom_LFSR(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int DefaultSeed;
extern PACKAGE TRandom* __fastcall RND(void);

}	/* namespace Fxcrng */
using namespace Fxcrng;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcrngHPP
