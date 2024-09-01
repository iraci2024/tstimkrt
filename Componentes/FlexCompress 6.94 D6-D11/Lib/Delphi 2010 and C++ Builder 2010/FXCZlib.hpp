// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxczlib.pas' rev: 21.00

#ifndef FxczlibHPP
#define FxczlibHPP

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

namespace Fxczlib
{
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef void * __cdecl (*alloc_func)(void * opaque, unsigned Items, unsigned Size);

typedef void __cdecl (*free_func)(void * opaque, void * address);

struct z_stream
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
class PASCALIMPLEMENTATION EZLibError : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EZLibError(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZLibError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZLibError(void) { }
	
};


class DELPHICLASS ECompressionError;
class PASCALIMPLEMENTATION ECompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* Exception.Create */ inline __fastcall ECompressionError(const System::UnicodeString Msg) : EZLibError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ECompressionError(int Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall ECompressionError(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ECompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECompressionError(int Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECompressionError(void) { }
	
};


class DELPHICLASS EDecompressionError;
class PASCALIMPLEMENTATION EDecompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* Exception.Create */ inline __fastcall EDecompressionError(const System::UnicodeString Msg) : EZLibError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EDecompressionError(int Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall EDecompressionError(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDecompressionError(int Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EDecompressionError(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char *ZLIB_VERSION;
static const ShortInt Z_NO_FLUSH = 0x0;
static const ShortInt Z_PARTIAL_FLUSH = 0x1;
static const ShortInt Z_SYNC_FLUSH = 0x2;
static const ShortInt Z_FULL_FLUSH = 0x3;
static const ShortInt Z_FINISH = 0x4;
static const ShortInt Z_BLOCK = 0x5;
static const ShortInt Z_TREES = 0x6;
static const ShortInt Z_OK = 0x0;
static const ShortInt Z_STREAM_END = 0x1;
static const ShortInt Z_NEED_DICT = 0x2;
static const ShortInt Z_ERRNO = -1;
static const ShortInt Z_STREAM_ERROR = -2;
static const ShortInt Z_DATA_ERROR = -3;
static const ShortInt Z_MEM_ERROR = -4;
static const ShortInt Z_BUF_ERROR = -5;
static const ShortInt Z_VERSION_ERROR = -6;
extern PACKAGE StaticArray<char *, 10> z_errmsg;
extern PACKAGE void * __cdecl zcAlloc(void * AppData, unsigned Items, unsigned Size);
extern PACKAGE void __cdecl zcFree(void * AppData, void * Block);
extern PACKAGE void __cdecl memset(void * P, System::Byte B, int Count);
extern PACKAGE void __cdecl memcpy(void * dest, void * Source, int Count);
extern PACKAGE int __fastcall CCheck(int code);
extern PACKAGE int __fastcall DCheck(int code);
extern PACKAGE void __fastcall ZLIBCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf, /* out */ int &OutBytes, System::Byte compMode = (System::Byte)(0x1));
extern PACKAGE void __fastcall ZLIBDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ void * &OutBuf, /* out */ int &OutBytes);
extern PACKAGE int __cdecl deflateInit_(z_stream &strm, int level, char * version, int stream_size);
extern PACKAGE int __cdecl deflate(z_stream &strm, int flush);
extern PACKAGE int __cdecl deflateEnd(z_stream &strm);
extern PACKAGE int __cdecl inflateInit_(z_stream &strm, char * version, int stream_size);
extern PACKAGE int __cdecl inflate(z_stream &strm, int flush);
extern PACKAGE int __cdecl inflateEnd(z_stream &strm);

}	/* namespace Fxczlib */
using namespace Fxczlib;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxczlibHPP
