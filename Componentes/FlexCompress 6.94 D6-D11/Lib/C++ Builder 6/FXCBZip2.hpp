// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCBZip2.pas' rev: 6.00

#ifndef FXCBZip2HPP
#define FXCBZip2HPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcbzip2
{
//-- type declarations -------------------------------------------------------
typedef void * __cdecl (*TAlloc)(void * opaque, int Items, int Size);

typedef void __cdecl (*TFree)(void * opaque, void * Block);

#pragma pack(push, 1)
struct TBZStreamRec
{
	char *next_in;
	unsigned avail_in;
	unsigned total_in_lo32;
	unsigned total_in_hi32;
	char *next_out;
	unsigned avail_out;
	unsigned total_out_lo32;
	unsigned total_out_hi32;
	void *state;
	TAlloc bzalloc;
	TFree bzfree;
	void *opaque;
} ;
#pragma pack(pop)

#pragma option push -b-
enum TBlockSize100k { bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 };
#pragma option pop

class DELPHICLASS EBZip2Error;
class PASCALIMPLEMENTATION EBZip2Error : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EBZip2Error(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EBZip2Error(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EBZip2Error(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EBZip2Error(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EBZip2Error(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EBZip2Error(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EBZip2Error(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EBZip2Error(void) { }
	#pragma option pop
	
};


class DELPHICLASS EBZCompressionError;
class PASCALIMPLEMENTATION EBZCompressionError : public EBZip2Error 
{
	typedef EBZip2Error inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EBZCompressionError(const AnsiString Msg) : EBZip2Error(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EBZCompressionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EBZCompressionError(int Ident)/* overload */ : EBZip2Error(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EBZCompressionError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EBZCompressionError(const AnsiString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EBZCompressionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EBZCompressionError(int Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EBZCompressionError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EBZDecompressionError;
class PASCALIMPLEMENTATION EBZDecompressionError : public EBZip2Error 
{
	typedef EBZip2Error inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EBZDecompressionError(const AnsiString Msg) : EBZip2Error(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EBZDecompressionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EBZDecompressionError(int Ident)/* overload */ : EBZip2Error(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EBZDecompressionError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EBZDecompressionError(const AnsiString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EBZDecompressionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EBZDecompressionError(int Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EBZDecompressionError(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall BZCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf, /* out */ int &OutBytes, int bzBlockSize = 0x9);
extern PACKAGE void __fastcall BZDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ void * &OutBuf, /* out */ int &OutBytes);
extern PACKAGE int __stdcall BZ2_bzBuffToBuffCompress(void * dest, int &destLen, void * Source, int sourceLen, int blockSize100k, int verbosity, int workFactor);

}	/* namespace Fxcbzip2 */
using namespace Fxcbzip2;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCBZip2
