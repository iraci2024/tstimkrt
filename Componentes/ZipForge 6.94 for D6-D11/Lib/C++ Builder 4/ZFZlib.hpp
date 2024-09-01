// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFZLib.pas' rev: 4.00

#ifndef ZFZLibHPP
#define ZFZLibHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfzlib
{
//-- type declarations -------------------------------------------------------
typedef Byte *PByte;

typedef void * __cdecl (*alloc_func)(void * opaque, unsigned Items, unsigned Size);

typedef void __cdecl (*free_func)(void * opaque, void * address);

#pragma pack(push, 4)
struct z_stream
{
	Byte *next_in;
	unsigned avail_in;
	unsigned total_in;
	Byte *next_out;
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
} ;
#pragma pack(pop)

typedef void * __cdecl (*TZAlloc)(void * opaque, unsigned Items, unsigned Size);

typedef void __cdecl (*TZFree)(void * opaque, void * address);

typedef z_stream  TZStreamRec;

class DELPHICLASS EZLibError;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION EZLibError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EZLibError(const AnsiString Msg) : Sysutils::Exception(Msg
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EZLibError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EZLibError(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(int Ident, const System::TVarRec * Args, 
		const int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EZLibError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(
		Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EZLibError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EZLibError(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS ECompressionError;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION ECompressionError : public EZLibError 
{
	typedef EZLibError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ECompressionError(const AnsiString Msg) : EZLibError(Msg) { }
		
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ECompressionError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ECompressionError(int Ident, Extended Dummy) : EZLibError(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ECompressionError(int Ident, const System::TVarRec * 
		Args, const int Args_Size) : EZLibError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ECompressionError(const AnsiString Msg, int AHelpContext
		) : EZLibError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ECompressionError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
		
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ECompressionError(int Ident, int AHelpContext) : EZLibError(
		Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ECompressionError(int Ident, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : EZLibError(Ident, Args, Args_Size, AHelpContext) { }
		
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ECompressionError(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS EDecompressionError;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION EDecompressionError : public EZLibError 
{
	typedef EZLibError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDecompressionError(const AnsiString Msg) : EZLibError(Msg
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDecompressionError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDecompressionError(int Ident, Extended Dummy) : EZLibError(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDecompressionError(int Ident, const System::TVarRec 
		* Args, const int Args_Size) : EZLibError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDecompressionError(const AnsiString Msg, int AHelpContext
		) : EZLibError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDecompressionError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
		
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDecompressionError(int Ident, int AHelpContext) : 
		EZLibError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDecompressionError(int Ident, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : EZLibError(Ident, Args, Args_Size, AHelpContext) { }
		
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDecompressionError(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char *ZLIB_VERSION;
static const Shortint Z_NO_FLUSH = 0x0;
static const Shortint Z_PARTIAL_FLUSH = 0x1;
static const Shortint Z_SYNC_FLUSH = 0x2;
static const Shortint Z_FULL_FLUSH = 0x3;
static const Shortint Z_FINISH = 0x4;
static const Shortint Z_BLOCK = 0x5;
static const Shortint Z_TREES = 0x6;
static const Shortint Z_OK = 0x0;
static const Shortint Z_STREAM_END = 0x1;
static const Shortint Z_NEED_DICT = 0x2;
static const Shortint Z_ERRNO = 0xffffffff;
static const Shortint Z_STREAM_ERROR = 0xfffffffe;
static const Shortint Z_DATA_ERROR = 0xfffffffd;
static const Shortint Z_MEM_ERROR = 0xfffffffc;
static const Shortint Z_BUF_ERROR = 0xfffffffb;
static const Shortint Z_VERSION_ERROR = 0xfffffffa;
extern PACKAGE char *z_errmsg[10];
extern PACKAGE void * __cdecl zcAlloc(void * AppData, unsigned Items, unsigned Size);
extern PACKAGE void __cdecl zcFree(void * AppData, void * Block);
extern PACKAGE void __cdecl memset(void * P, Byte B, int Count);
extern PACKAGE void __cdecl memcpy(void * dest, void * Source, int Count);
extern PACKAGE int __fastcall CCheck(int code);
extern PACKAGE int __fastcall DCheck(int code);
extern PACKAGE void __fastcall ZLIBCompressBuf(const void * InBuf, int InBytes, /* out */ void * &OutBuf
	, /* out */ int &OutBytes, Byte compMode);
extern PACKAGE void __fastcall ZLIBDecompressBuf(const void * InBuf, int InBytes, int OutEstimate, /* out */ 
	void * &OutBuf, /* out */ int &OutBytes);
extern PACKAGE int __cdecl deflateInit_(z_stream &strm, int level, char * version, int stream_size);
	
extern PACKAGE int __cdecl deflate(z_stream &strm, int flush);
extern PACKAGE int __cdecl deflateEnd(z_stream &strm);
extern PACKAGE int __cdecl inflateInit_(z_stream &strm, char * version, int stream_size);
extern PACKAGE int __cdecl inflate(z_stream &strm, int flush);
extern PACKAGE int __cdecl inflateEnd(z_stream &strm);

}	/* namespace Zfzlib */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfzlib;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFZLib
