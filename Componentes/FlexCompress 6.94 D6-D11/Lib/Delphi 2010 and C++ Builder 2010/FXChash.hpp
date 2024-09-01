// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxchash.pas' rev: 21.00

#ifndef FxchashHPP
#define FxchashHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxchash
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum THashAlgorithm { _MD4, _MD5, _RIPEMD160, _SHA1, _SHA224, _SHA256, _SHA384, _SHA512, _Whirlpool };
#pragma option pop

typedef StaticArray<int, 16> THashState;

typedef StaticArray<System::Byte, 128> THashBuffer;

typedef StaticArray<System::Byte, 64> THashDigest;

typedef StaticArray<int, 32> THashBuf32;

typedef StaticArray<int, 16> THashDig32;

#pragma pack(push,1)
struct THashContext
{
	
public:
	THashState Hash;
	StaticArray<int, 4> MLen;
	THashBuffer Buffer;
	int Index;
};
#pragma pack(pop)


typedef StaticArray<System::Byte, 16> TMD4Digest;

typedef StaticArray<System::Byte, 16> TMD5Digest;

typedef StaticArray<System::Byte, 20> TRMD160Digest;

typedef StaticArray<System::Byte, 20> TSHA1Digest;

typedef StaticArray<System::Byte, 28> TSHA224Digest;

typedef StaticArray<System::Byte, 32> TSHA256Digest;

typedef StaticArray<System::Byte, 48> TSHA384Digest;

typedef StaticArray<System::Byte, 64> TSHA512Digest;

typedef StaticArray<System::Byte, 64> TWhirlDigest;

typedef void __fastcall (*HashInitProc)(THashContext &Context);

typedef void __fastcall (*HashFinalProc)(THashContext &Context, System::Byte *Digest);

typedef void __fastcall (*HashUpdateXLProc)(THashContext &Context, void * Msg, int Len);

typedef StaticArray<int, 9> TOID_Vec;

typedef TOID_Vec *POID_Vec;

typedef SmallString<19>  THashName;

struct THashDesc;
typedef THashDesc *PHashDesc;

#pragma pack(push,1)
struct THashDesc
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
	StaticArray<System::Byte, 26> HReserved;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
static const Byte MaxBlockLen = 0x80;
static const ShortInt MaxDigestLen = 0x40;
static const ShortInt MaxStateLen = 0x10;
static const ShortInt MaxOIDLen = 0x9;
static const Word C_HashSig = 0x3d7a;
static const int C_HashVers = 0x10003;
static const THashAlgorithm C_MinHash = (THashAlgorithm)(0);
static const THashAlgorithm C_MaxHash = (THashAlgorithm)(8);
extern PACKAGE void __fastcall RegisterHash(THashAlgorithm AlgId, PHashDesc PHash);
extern PACKAGE PHashDesc __fastcall FindHash_by_ID(THashAlgorithm AlgoID);
extern PACKAGE PHashDesc __fastcall FindHash_by_Name(THashName &AlgoName);
extern PACKAGE void __fastcall HashUpdate(PHashDesc PHash, THashContext &Context, void * Msg, System::Word Len);
extern PACKAGE void __fastcall HashFullXL(PHashDesc PHash, System::Byte *Digest, void * Msg, int Len);
extern PACKAGE void __fastcall HashFull(PHashDesc PHash, System::Byte *Digest, void * Msg, System::Word Len);
extern PACKAGE void __fastcall HashFile(const System::ShortString &fname, PHashDesc PHash, System::Byte *Digest, void *buf, System::Word bsize, System::Word &Err);

}	/* namespace Fxchash */
using namespace Fxchash;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxchashHPP
