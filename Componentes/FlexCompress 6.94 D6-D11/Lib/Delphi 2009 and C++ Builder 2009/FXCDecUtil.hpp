// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcdecutil.pas' rev: 20.00

#ifndef FxcdecutilHPP
#define FxcdecutilHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcdecutil
{
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef unsigned *PInteger;

typedef System::Word *PWord;

typedef StaticArray<unsigned, 1024> TIntArray;

typedef TIntArray *PIntArray;

class DELPHICLASS EProtection;
class PASCALIMPLEMENTATION EProtection : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EProtection(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EProtection(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EProtection(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EProtection(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EProtection(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EProtection(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EProtection(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EProtection(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EProtection(void) { }
	
};


class DELPHICLASS EStringFormat;
class PASCALIMPLEMENTATION EStringFormat : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EStringFormat(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStringFormat(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EStringFormat(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EStringFormat(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EStringFormat(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStringFormat(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStringFormat(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStringFormat(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStringFormat(void) { }
	
};


#pragma option push -b-
enum TPAction { paEncode, paDecode, paScramble, paCalc, paWipe };
#pragma option pop

typedef Set<TPAction, paEncode, paWipe>  TPActions;

class DELPHICLASS TProtection;
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
	virtual void __fastcall CodeStream(Classes::TStream* Source, Classes::TStream* Dest, int DataSize, TPAction Action);
	virtual void __fastcall CodeFile(const System::AnsiString Source, const System::AnsiString Dest, TPAction Action);
	virtual System::AnsiString __fastcall CodeString(const System::AnsiString Source, TPAction Action, int Format);
	virtual int __fastcall CodeBuffer(void *Buffer, int BufferSize, TPAction Action);
	__property TProtection* Protection = {read=GetProtection, write=SetProtection};
	__property TPActions Actions = {read=FActions, write=FActions, default=31};
};


typedef TMetaClass* TStringFormatClass;

class DELPHICLASS TStringFormat;
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


class DELPHICLASS TStringFormat_HEX;
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


class DELPHICLASS TStringFormat_HEXL;
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


class DELPHICLASS TStringFormat_MIME64;
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


class DELPHICLASS TStringFormat_UU;
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


class DELPHICLASS TStringFormat_XX;
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


typedef void __fastcall (__closure *TProgressEvent)(System::TObject* Sender, int Current, int Maximal);

//-- var, const, procedure ---------------------------------------------------
static const ShortInt fmtDEFAULT = -1;
static const ShortInt fmtNONE = 0x0;
static const ShortInt fmtCOPY = 0x1;
static const ShortInt fmtHEX = 0x10;
static const Word fmtHEXL = 0x3f8;
static const Word fmtMIME64 = 0x1064;
static const Word fmtUU = 0x5555;
static const Word fmtXX = 0x5858;
extern PACKAGE bool InitTestIsOk;
extern PACKAGE System::Word IdentityBase;
extern PACKAGE unsigned __fastcall (*SwapInteger)(unsigned Value);
extern PACKAGE void __fastcall (*SwapIntegerBuffer)(void * Source, void * Dest, int Count);
extern PACKAGE TProgressEvent Progress;
extern PACKAGE System::Comp __fastcall PerfCounter(void);
extern PACKAGE System::Comp __fastcall PerfFreq(void);
extern PACKAGE int __fastcall DefaultStringFormat(void);
extern PACKAGE void __fastcall SetDefaultStringFormat(int Format);
extern PACKAGE int __fastcall CPUType(void);
extern PACKAGE bool __fastcall IsObject(void * AObject, System::TClass AClass);
extern PACKAGE unsigned __fastcall ROL(unsigned Value, int Shift);
extern PACKAGE unsigned __fastcall ROLADD(unsigned Value, unsigned Add, int Shift);
extern PACKAGE unsigned __fastcall ROLSUB(unsigned Value, unsigned Sub, int Shift);
extern PACKAGE unsigned __fastcall ROR(unsigned Value, int Shift);
extern PACKAGE unsigned __fastcall RORADD(unsigned Value, unsigned Add, int Shift);
extern PACKAGE unsigned __fastcall RORSUB(unsigned Value, unsigned Sub, int Shift);
extern PACKAGE unsigned __fastcall SwapBits(unsigned Value);
extern PACKAGE int __fastcall LSBit(int Value);
extern PACKAGE int __fastcall MSBit(int Value);
extern PACKAGE int __fastcall OneBit(int Value);
extern PACKAGE int __fastcall MemCompare(void * P1, void * P2, int Size);
extern PACKAGE void __fastcall XORBuffers(void * I1, void * I2, int Size, void * Dest);
extern PACKAGE void __fastcall DoProgress(System::TObject* Sender, int Current, int Maximal);
extern PACKAGE TStringFormatClass __fastcall StringFormat(int Format);
extern PACKAGE System::AnsiString __fastcall StrToFormat(char * Value, int Len, int Format);
extern PACKAGE System::AnsiString __fastcall FormatToStr(char * Value, int Len, int Format);
extern PACKAGE System::AnsiString __fastcall ConvertFormat(char * Value, int Len, int FromFormat, int ToFormat);
extern PACKAGE bool __fastcall IsValidFormat(char * Value, int Len, int Format);
extern PACKAGE bool __fastcall IsValidString(char * Value, int Len, int Format);
extern PACKAGE void __fastcall RegisterStringFormats(TStringFormatClass const *StringFormats, const int StringFormats_Size);
extern PACKAGE void __fastcall GetStringFormats(Classes::TStrings* Strings);
extern PACKAGE System::AnsiString __fastcall InsertCR(const System::AnsiString Value, int BlockSize);
extern PACKAGE System::AnsiString __fastcall DeleteCR(const System::AnsiString Value);
extern PACKAGE System::AnsiString __fastcall InsertBlocks(const System::AnsiString Value, const System::AnsiString BlockStart, const System::AnsiString BlockEnd, int BlockSize);
extern PACKAGE System::AnsiString __fastcall RemoveBlocks(const System::AnsiString Value, const System::AnsiString BlockStart, const System::AnsiString BlockEnd);
extern PACKAGE System::AnsiString __fastcall GetShortClassName(System::TClass Value);
extern PACKAGE int __fastcall RndXORBuffer(int Seed, void *Buffer, int Size);
extern PACKAGE int __fastcall RndTimeSeed(void);
extern PACKAGE System::Word __fastcall CRC16(System::Word CRC, void * Data, unsigned DataSize);
extern PACKAGE char * __fastcall GetTestVector(void);

}	/* namespace Fxcdecutil */
using namespace Fxcdecutil;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcdecutilHPP
