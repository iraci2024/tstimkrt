// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCHash.pas' rev: 31.00 (Windows)

#ifndef FxchashHPP
#define FxchashHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxchash
{
//-- forward type declarations -----------------------------------------------
struct THashContext;
struct THashDesc;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM THashAlgorithm : unsigned char { _MD4, _MD5, _RIPEMD160, _SHA1, _SHA224, _SHA256, _SHA384, _SHA512, _Whirlpool };

typedef System::StaticArray<int, 16> THashState;

typedef System::StaticArray<System::Byte, 128> THashBuffer;

typedef System::StaticArray<System::Byte, 64> THashDigest;

typedef System::StaticArray<int, 32> THashBuf32;

typedef System::StaticArray<int, 16> THashDig32;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THashContext
{
public:
	THashState Hash;
	System::StaticArray<int, 4> MLen;
	THashBuffer Buffer;
	int Index;
};
#pragma pack(pop)


typedef System::StaticArray<System::Byte, 16> TMD4Digest;

typedef System::StaticArray<System::Byte, 16> TMD5Digest;

typedef System::StaticArray<System::Byte, 20> TRMD160Digest;

typedef System::StaticArray<System::Byte, 20> TSHA1Digest;

typedef System::StaticArray<System::Byte, 28> TSHA224Digest;

typedef System::StaticArray<System::Byte, 32> TSHA256Digest;

typedef System::StaticArray<System::Byte, 48> TSHA384Digest;

typedef System::StaticArray<System::Byte, 64> TSHA512Digest;

typedef System::StaticArray<System::Byte, 64> TWhirlDigest;

typedef void __fastcall (*HashInitProc)(THashContext &Context);

typedef void __fastcall (*HashFinalProc)(THashContext &Context, THashDigest &Digest);

typedef void __fastcall (*HashUpdateXLProc)(THashContext &Context, void * Msg, int Len);

typedef System::StaticArray<int, 9> TOID_Vec;

typedef TOID_Vec *POID_Vec;

typedef System::SmallString<19> THashName;

typedef THashDesc *PHashDesc;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THashDesc
{
public:
	System::Word HSig;
	System::Word HDSize;
	int HDVersion;
	System::Word HBlockLen;
	System::Word HDigestlen;
	HashInitProc HInit;
	HashFinalProc HFinal;
	HashUpdateXLProc HUpdateXL;
	int HAlgNum;
	THashName HName;
	TOID_Vec *HPtrOID;
	System::Word HLenOID;
	System::StaticArray<System::Byte, 26> HReserved;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
static const System::Byte MaxBlockLen = System::Byte(0x80);
static const System::Int8 MaxDigestLen = System::Int8(0x40);
static const System::Int8 MaxStateLen = System::Int8(0x10);
static const System::Int8 MaxOIDLen = System::Int8(0x9);
static const System::Word C_HashSig = System::Word(0x3d7a);
static const int C_HashVers = int(0x10003);
static const THashAlgorithm C_MinHash = (THashAlgorithm)(0);
static const THashAlgorithm C_MaxHash = (THashAlgorithm)(8);
extern DELPHI_PACKAGE void __fastcall RegisterHash(THashAlgorithm AlgId, PHashDesc PHash);
extern DELPHI_PACKAGE PHashDesc __fastcall FindHash_by_ID(THashAlgorithm AlgoID);
extern DELPHI_PACKAGE PHashDesc __fastcall FindHash_by_Name(THashName &AlgoName);
extern DELPHI_PACKAGE void __fastcall HashUpdate(PHashDesc PHash, THashContext &Context, void * Msg, System::Word Len);
extern DELPHI_PACKAGE void __fastcall HashFullXL(PHashDesc PHash, THashDigest &Digest, void * Msg, int Len);
extern DELPHI_PACKAGE void __fastcall HashFull(PHashDesc PHash, THashDigest &Digest, void * Msg, System::Word Len);
extern DELPHI_PACKAGE void __fastcall HashFile(const System::ShortString &fname, PHashDesc PHash, THashDigest &Digest, void *buf, System::Word bsize, System::Word &Err);
}	/* namespace Fxchash */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCHASH)
using namespace Fxchash;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxchashHPP
