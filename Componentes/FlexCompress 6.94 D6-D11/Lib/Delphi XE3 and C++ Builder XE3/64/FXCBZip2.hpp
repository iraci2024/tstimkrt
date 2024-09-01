// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCBZip2.pas' rev: 24.00 (Windows)

#ifndef Fxcbzip2HPP
#define Fxcbzip2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcbzip2
{
//-- type declarations -------------------------------------------------------
typedef void * __cdecl (*TAlloc)(void * opaque, int Items, int Size);

typedef void __cdecl (*TFree)(void * opaque, void * Block);

#pragma pack(push,1)
struct DECLSPEC_DRECORD TBZStreamRec
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


enum DECLSPEC_DENUM TBlockSize100k : unsigned char { bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 };

class DELPHICLASS EBZip2Error;
class PASCALIMPLEMENTATION EBZip2Error : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZip2Error(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZip2Error(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZip2Error(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZip2Error(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZip2Error(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZip2Error(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZip2Error(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZip2Error(void) { }
	
};


class DELPHICLASS EBZCompressionError;
class PASCALIMPLEMENTATION EBZCompressionError : public EBZip2Error
{
	typedef EBZip2Error inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg) : EBZip2Error(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZCompressionError(NativeUInt Ident)/* overload */ : EBZip2Error(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec)/* overload */ : EBZip2Error(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZCompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZCompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZCompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZCompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZCompressionError(void) { }
	
};


class DELPHICLASS EBZDecompressionError;
class PASCALIMPLEMENTATION EBZDecompressionError : public EBZip2Error
{
	typedef EBZip2Error inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg) : EBZip2Error(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EBZip2Error(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EBZDecompressionError(NativeUInt Ident)/* overload */ : EBZip2Error(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec)/* overload */ : EBZip2Error(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, int AHelpContext) : EBZip2Error(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EBZip2Error(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZDecompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EBZip2Error(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBZDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EBZip2Error(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBZDecompressionError(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall BZCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf, /* out */ int &OutBytes, int bzBlockSize = 0x9);
extern DELPHI_PACKAGE void __fastcall BZDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ void * &OutBuf, /* out */ int &OutBytes);
extern DELPHI_PACKAGE int __stdcall BZ2_bzBuffToBuffCompress(void * dest, int &destLen, void * Source, int sourceLen, int blockSize100k, int verbosity, int workFactor);
}	/* namespace Fxcbzip2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCBZIP2)
using namespace Fxcbzip2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcbzip2HPP
