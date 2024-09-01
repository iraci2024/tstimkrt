// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFDecUtil.pas' rev: 32.00 (Windows)

#ifndef ZfdecutilHPP
#define ZfdecutilHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zfdecutil
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EProtection;
class DELPHICLASS EStringFormat;
class DELPHICLASS TProtection;
class DELPHICLASS TStringFormat;
class DELPHICLASS TStringFormat_HEX;
class DELPHICLASS TStringFormat_HEXL;
class DELPHICLASS TStringFormat_MIME64;
class DELPHICLASS TStringFormat_UU;
class DELPHICLASS TStringFormat_XX;
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef unsigned *PInteger;

typedef System::Word *PWord;

typedef System::StaticArray<unsigned, 1024> TIntArray;

typedef TIntArray *PIntArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EProtection : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EProtection(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EProtection(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EProtection(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EProtection(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EProtection(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EProtection(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EProtection(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EProtection(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EProtection(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EProtection(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EProtection(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EProtection(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EProtection(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStringFormat : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EStringFormat(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStringFormat(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStringFormat(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStringFormat(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStringFormat(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStringFormat(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStringFormat(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStringFormat(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStringFormat(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStringFormat(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStringFormat(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStringFormat(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStringFormat(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TPAction : unsigned char { paEncode, paDecode, paScramble, paCalc, paWipe };

typedef System::Set<TPAction, TPAction::paEncode, TPAction::paWipe> TPActions;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TProtection : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TProtection* FProtection;
	TPActions FActions;
	TProtection* __fastcall GetProtection(void);
	void __fastcall SetProtection(TProtection* Value);
	
protected:
	virtual void __fastcall CodeInit(TPAction Action);
	virtual void __fastcall CodeDone(TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, TPAction Action);
	
public:
	__fastcall TProtection(TProtection* AProtection);
	__fastcall virtual ~TProtection(void);
	__classmethod System::Word __fastcall Identity();
	int __fastcall Release(void);
	int __fastcall AddRef(void);
	virtual void __fastcall CodeStream(System::Classes::TStream* Source, System::Classes::TStream* Dest, int DataSize, TPAction Action);
	virtual void __fastcall CodeFile(const System::AnsiString Source, const System::AnsiString Dest, TPAction Action);
	virtual System::AnsiString __fastcall CodeString(const System::AnsiString Source, TPAction Action, int Format);
	virtual int __fastcall CodeBuffer(void *Buffer, int BufferSize, TPAction Action);
	__property TProtection* Protection = {read=GetProtection, write=SetProtection};
	__property TPActions Actions = {read=FActions, write=FActions, default=31};
};

#pragma pack(pop)

typedef System::TMetaClass* TStringFormatClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall ToStr(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall StrTo(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual bool __fastcall IsValid(char * Value, int Len, bool ToStr);
public:
	/* TObject.Create */ inline __fastcall TStringFormat(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat_HEX : public TStringFormat
{
	typedef TStringFormat inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall ToStr(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall StrTo(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual bool __fastcall IsValid(char * Value, int Len, bool ToStr);
	__classmethod virtual char * __fastcall CharTable();
public:
	/* TObject.Create */ inline __fastcall TStringFormat_HEX(void) : TStringFormat() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat_HEX(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat_HEXL : public TStringFormat_HEX
{
	typedef TStringFormat_HEX inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual char * __fastcall CharTable();
public:
	/* TObject.Create */ inline __fastcall TStringFormat_HEXL(void) : TStringFormat_HEX() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat_HEXL(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat_MIME64 : public TStringFormat_HEX
{
	typedef TStringFormat_HEX inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall ToStr(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall StrTo(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual char * __fastcall CharTable();
public:
	/* TObject.Create */ inline __fastcall TStringFormat_MIME64(void) : TStringFormat_HEX() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat_MIME64(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat_UU : public TStringFormat
{
	typedef TStringFormat inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall ToStr(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall StrTo(char * Value, int Len);
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual bool __fastcall IsValid(char * Value, int Len, bool ToStr);
	__classmethod virtual char * __fastcall CharTable();
public:
	/* TObject.Create */ inline __fastcall TStringFormat_UU(void) : TStringFormat() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat_UU(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringFormat_XX : public TStringFormat_UU
{
	typedef TStringFormat_UU inherited;
	
public:
	__classmethod virtual System::AnsiString __fastcall Name();
	__classmethod virtual int __fastcall Format();
	__classmethod virtual char * __fastcall CharTable();
public:
	/* TObject.Create */ inline __fastcall TStringFormat_XX(void) : TStringFormat_UU() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStringFormat_XX(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TProgressEvent)(System::TObject* Sender, int Current, int Maximal);

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 fmtDEFAULT = System::Int8(-1);
static const System::Int8 fmtNONE = System::Int8(0x0);
static const System::Int8 fmtCOPY = System::Int8(0x1);
static const System::Int8 fmtHEX = System::Int8(0x10);
static const System::Word fmtHEXL = System::Word(0x3f8);
static const System::Word fmtMIME64 = System::Word(0x1064);
static const System::Word fmtUU = System::Word(0x5555);
static const System::Word fmtXX = System::Word(0x5858);
extern DELPHI_PACKAGE bool InitTestIsOk;
extern DELPHI_PACKAGE System::Word IdentityBase;
extern DELPHI_PACKAGE unsigned __fastcall (*SwapInteger)(unsigned Value);
extern DELPHI_PACKAGE void __fastcall (*SwapIntegerBuffer)(void * Source, void * Dest, int Count);
extern DELPHI_PACKAGE TProgressEvent Progress;
extern DELPHI_PACKAGE __int64 __fastcall PerfCounter(void);
extern DELPHI_PACKAGE __int64 __fastcall PerfFreq(void);
extern DELPHI_PACKAGE int __fastcall DefaultStringFormat(void);
extern DELPHI_PACKAGE void __fastcall SetDefaultStringFormat(int Format);
extern DELPHI_PACKAGE int __fastcall CPUType(void);
extern DELPHI_PACKAGE bool __fastcall IsObject(void * AObject, System::TClass AClass);
extern DELPHI_PACKAGE unsigned __fastcall ROL(unsigned Value, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall ROLADD(unsigned Value, unsigned Add, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall ROLSUB(unsigned Value, unsigned Sub, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall ROR(unsigned Value, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall RORADD(unsigned Value, unsigned Add, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall RORSUB(unsigned Value, unsigned Sub, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall SwapBits(unsigned Value);
extern DELPHI_PACKAGE int __fastcall LSBit(int Value);
extern DELPHI_PACKAGE int __fastcall MSBit(int Value);
extern DELPHI_PACKAGE int __fastcall OneBit(int Value);
extern DELPHI_PACKAGE int __fastcall MemCompare(void * P1, void * P2, int Size);
extern DELPHI_PACKAGE void __fastcall XORBuffers(void * I1, void * I2, int Size, void * Dest);
extern DELPHI_PACKAGE void __fastcall DoProgress(System::TObject* Sender, int Current, int Maximal);
extern DELPHI_PACKAGE TStringFormatClass __fastcall StringFormat(int Format);
extern DELPHI_PACKAGE System::AnsiString __fastcall StrToFormat(char * Value, int Len, int Format);
extern DELPHI_PACKAGE System::AnsiString __fastcall FormatToStr(char * Value, int Len, int Format);
extern DELPHI_PACKAGE System::AnsiString __fastcall ConvertFormat(char * Value, int Len, int FromFormat, int ToFormat);
extern DELPHI_PACKAGE bool __fastcall IsValidFormat(char * Value, int Len, int Format);
extern DELPHI_PACKAGE bool __fastcall IsValidString(char * Value, int Len, int Format);
extern DELPHI_PACKAGE void __fastcall RegisterStringFormats(TStringFormatClass const *StringFormats, const int StringFormats_High);
extern DELPHI_PACKAGE void __fastcall GetStringFormats(System::Classes::TStrings* Strings);
extern DELPHI_PACKAGE System::AnsiString __fastcall InsertCR(const System::AnsiString Value, int BlockSize);
extern DELPHI_PACKAGE System::AnsiString __fastcall DeleteCR(const System::AnsiString Value);
extern DELPHI_PACKAGE System::AnsiString __fastcall InsertBlocks(const System::AnsiString Value, const System::AnsiString BlockStart, const System::AnsiString BlockEnd, int BlockSize);
extern DELPHI_PACKAGE System::AnsiString __fastcall RemoveBlocks(const System::AnsiString Value, const System::AnsiString BlockStart, const System::AnsiString BlockEnd);
extern DELPHI_PACKAGE System::AnsiString __fastcall GetShortClassName(System::TClass Value);
extern DELPHI_PACKAGE int __fastcall RndXORBuffer(int Seed, void *Buffer, int Size);
extern DELPHI_PACKAGE int __fastcall RndTimeSeed(void);
extern DELPHI_PACKAGE System::Word __fastcall CRC16(System::Word CRC, void * Data, unsigned DataSize);
extern DELPHI_PACKAGE char * __fastcall GetTestVector(void);
}	/* namespace Zfdecutil */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFDECUTIL)
using namespace Zfdecutil;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfdecutilHPP
