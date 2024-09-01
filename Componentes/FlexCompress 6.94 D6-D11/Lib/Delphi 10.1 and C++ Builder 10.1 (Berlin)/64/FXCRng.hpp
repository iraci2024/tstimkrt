// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCRNG.pas' rev: 31.00 (Windows)

#ifndef FxcrngHPP
#define FxcrngHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <FXCDecUtil.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxcrng
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ERandom;
class DELPHICLASS TRandom;
class DELPHICLASS TRandom_LFSR;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION ERandom : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ERandom(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ERandom(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ERandom(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ERandom(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ERandom(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ERandom(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ERandom(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ERandom(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERandom(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERandom(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERandom(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERandom(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ERandom(void) { }
	
};


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
	virtual System::AnsiString __fastcall GetState(void);
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
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(const System::AnsiString FileName);
	void __fastcall LoadFromFile(const System::AnsiString FileName);
	__property int Count = {read=FCount, write=FCount, nodefault};
	__property int Size = {read=FSize, write=SetSize, nodefault};
	__property int BasicSeed = {read=FBasicSeed, write=FBasicSeed, nodefault};
	__property System::AnsiString State = {read=GetState, write=SetState};
};


class PASCALIMPLEMENTATION TRandom_LFSR : public TRandom
{
	typedef TRandom inherited;
	
private:
	int FPtr;
	int FLast;
	System::StaticArray<System::Word, 256> FTable;
	System::StaticArray<System::Byte, 256> FRegister;
	void __fastcall (*FFunc)(void * Self, void *Buffer, int Size);
	
protected:
	virtual void __fastcall SetSize(int Value);
	virtual System::AnsiString __fastcall GetState(void);
	virtual void __fastcall SetState(System::AnsiString Value);
	
public:
	virtual void __fastcall Seed(const void *ABuffer, int ASize);
	virtual void __fastcall Buffer(void *ABuffer, int ASize);
public:
	/* TRandom.Create */ inline __fastcall virtual TRandom_LFSR(const System::AnsiString APassword, int ASize, bool ARandomize, Fxcdecutil::TProtection* AProtection) : TRandom(APassword, ASize, ARandomize, AProtection) { }
	/* TRandom.Destroy */ inline __fastcall virtual ~TRandom_LFSR(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int DefaultSeed;
extern DELPHI_PACKAGE TRandom* __fastcall RND(void);
}	/* namespace Fxcrng */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCRNG)
using namespace Fxcrng;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcrngHPP
