// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcbzip2.pas' rev: 21.00

#ifndef Fxcbzip2HPP
#define Fxcbzip2HPP

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

namespace Fxcbzip2
{
//-- type declarations -------------------------------------------------------
typedef void * __cdecl (*TAlloc)(void * opaque, int Items, int Size);

typedef void __cdecl (*TFree)(void * opaque, void * Block);

#pragma pack(push,1)
struct TBZStreamRec
{
	
public:
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
};
#pragma pack(pop)


#pragma option push -b-
enum TBlockSize100k { bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 };
#pragma option pop

class DELPHICLASS EBZip2Error;
class PASCALIMPLEMENTATION EBZip2Error : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZip2Error(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZip2Error(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZip2Error(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZip2Error(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZip2Error(void) { }
	
};


class DELPHICLASS EBZCompressionError;
class PASCALIMPLEMENTATION EBZCompressionError : public EBZip2Error
{
	typedef EBZip2Error inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg) : EBZip2Error(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZCompressionError(int Ident)/* overload */ : EBZip2Error(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZCompressionError(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZCompressionError(int Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZCompressionError(void) { }
	
};


class DELPHICLASS EBZDecompressionError;
class PASCALIMPLEMENTATION EBZDecompressionError : public EBZip2Error
{
	typedef EBZip2Error inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg) : EBZip2Error(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZDecompressionError(int Ident)/* overload */ : EBZip2Error(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZDecompressionError(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZDecompressionError(int Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZDecompressionError(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall BZCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf, /* out */ int &OutBytes, int bzBlockSize = 0x9);
extern PACKAGE void __fastcall BZDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ void * &OutBuf, /* out */ int &OutBytes);
extern PACKAGE int __stdcall BZ2_bzBuffToBuffCompress(void * dest, int &destLen, void * Source, int sourceLen, int blockSize100k, int verbosity, int workFactor);

}	/* namespace Fxcbzip2 */
using namespace Fxcbzip2;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcbzip2HPP
