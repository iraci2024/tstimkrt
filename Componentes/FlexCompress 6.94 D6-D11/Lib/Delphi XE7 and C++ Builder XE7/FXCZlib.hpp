// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCZLib.pas' rev: 28.00 (Windows)

#ifndef FxczlibHPP
#define FxczlibHPP

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

namespace Fxczlib
{
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef void * __cdecl (*alloc_func)(void * opaque, unsigned Items, unsigned Size);

typedef void __cdecl (*free_func)(void * opaque, void * address);

struct DECLSPEC_DRECORD z_stream
{
public:
	System::Byte *next_in;
	unsigned avail_in;
	unsigned total_in;
	System::Byte *next_out;
	unsigned avail_out;
	unsigned total_out;
	char *msg;
	void *state;
	alloc_func zalloc;
	free_func zfree;
	void *opaque;
	int data_type;
	unsigned adler;
	unsigned reserved;
};


typedef alloc_func TZAlloc;

typedef free_func TZFree;

typedef z_stream TZStreamRec;

class DELPHICLASS EZLibError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZLibError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EZLibError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EZLibError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZLibError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZLibError(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS ECompressionError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ECompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* Exception.Create */ inline __fastcall ECompressionError(const System::UnicodeString Msg) : EZLibError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EZLibError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ECompressionError(NativeUInt Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ECompressionError(System::PResStringRec ResStringRec)/* overload */ : EZLibError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ECompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EZLibError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ECompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EZLibError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ECompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EZLibError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EZLibError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EZLibError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECompressionError(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EDecompressionError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EDecompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* Exception.Create */ inline __fastcall EDecompressionError(const System::UnicodeString Msg) : EZLibError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EZLibError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EDecompressionError(NativeUInt Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EDecompressionError(System::PResStringRec ResStringRec)/* overload */ : EZLibError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EZLibError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EZLibError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EZLibError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDecompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDecompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EZLibError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EZLibError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EDecompressionError(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE char *ZLIB_VERSION;
static const System::Int8 Z_NO_FLUSH = System::Int8(0x0);
static const System::Int8 Z_PARTIAL_FLUSH = System::Int8(0x1);
static const System::Int8 Z_SYNC_FLUSH = System::Int8(0x2);
static const System::Int8 Z_FULL_FLUSH = System::Int8(0x3);
static const System::Int8 Z_FINISH = System::Int8(0x4);
static const System::Int8 Z_BLOCK = System::Int8(0x5);
static const System::Int8 Z_TREES = System::Int8(0x6);
static const System::Int8 Z_OK = System::Int8(0x0);
static const System::Int8 Z_STREAM_END = System::Int8(0x1);
static const System::Int8 Z_NEED_DICT = System::Int8(0x2);
static const System::Int8 Z_ERRNO = System::Int8(-1);
static const System::Int8 Z_STREAM_ERROR = System::Int8(-2);
static const System::Int8 Z_DATA_ERROR = System::Int8(-3);
static const System::Int8 Z_MEM_ERROR = System::Int8(-4);
static const System::Int8 Z_BUF_ERROR = System::Int8(-5);
static const System::Int8 Z_VERSION_ERROR = System::Int8(-6);
extern DELPHI_PACKAGE System::StaticArray<char *, 10> z_errmsg;
extern DELPHI_PACKAGE void * __cdecl zcAlloc(void * AppData, unsigned Items, unsigned Size);
extern DELPHI_PACKAGE void __cdecl zcFree(void * AppData, void * Block);
extern DELPHI_PACKAGE void __cdecl memset(void * P, System::Byte B, int Count);
extern DELPHI_PACKAGE void __cdecl memcpy(void * dest, void * Source, int Count);
extern DELPHI_PACKAGE int __fastcall CCheck(int code);
extern DELPHI_PACKAGE int __fastcall DCheck(int code);
extern DELPHI_PACKAGE void __fastcall ZLIBCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf, /* out */ int &OutBytes, System::Byte compMode = (System::Byte)(0x1));
extern DELPHI_PACKAGE void __fastcall ZLIBDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ void * &OutBuf, /* out */ int &OutBytes);
extern DELPHI_PACKAGE int __cdecl deflateInit_(z_stream &strm, int level, char * version, int stream_size);
extern DELPHI_PACKAGE int __cdecl deflate(z_stream &strm, int flush);
extern DELPHI_PACKAGE int __cdecl deflateEnd(z_stream &strm);
extern DELPHI_PACKAGE int __cdecl inflateInit_(z_stream &strm, char * version, int stream_size);
extern DELPHI_PACKAGE int __cdecl inflate(z_stream &strm, int flush);
extern DELPHI_PACKAGE int __cdecl inflateEnd(z_stream &strm);
}	/* namespace Fxczlib */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCZLIB)
using namespace Fxczlib;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxczlibHPP
