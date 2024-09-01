// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfhash.pas' rev: 10.00

#ifndef ZfhashHPP
#define ZfhashHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfhash
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum THashAlgorithm { _MD4, _MD5, _RIPEMD160, _SHA1, _SHA224, _SHA256, _SHA384, _SHA512, _Whirlpool };
#pragma option pop

typedef int THashState[16];

typedef Byte THashBuffer[128];

typedef Byte THashDigest[64];

typedef int THashBuf32[32];

typedef int THashDig32[16];

#pragma pack(push,1)
struct THashContext
{
	
public:
	int Hash[16];
	int MLen[4];
	Byte Buffer[128];
	int Index;
} ;
#pragma pack(pop)

typedef Byte TMD4Digest[16];

typedef Byte TMD5Digest[16];

typedef Byte TRMD160Digest[20];

typedef Byte TSHA1Digest[20];

typedef Byte TSHA224Digest[28];

typedef Byte TSHA256Digest[32];

typedef Byte TSHA384Digest[48];

typedef Byte TSHA512Digest[64];

typedef Byte TWhirlDigest[64];

typedef void __fastcall (*HashInitProc)(THashContext &Context);

typedef void __fastcall (*HashFinalProc)(THashContext &Context, Byte * Digest);

typedef void __fastcall (*HashUpdateXLProc)(THashContext &Context, void * Msg, int Len);

typedef int TOID_Vec[9];

typedef int *POID_Vec;

typedef SmallString<19>  THashName;

struct THashDesc;
typedef THashDesc *PHashDesc;

#pragma pack(push,1)
struct THashDesc
{
	
public:
	Word HSig;
	Word HDSize;
	int HDVersion;
	Word HBlockLen;
	Word HDigestlen;
	HashInitProc HInit;
	HashFinalProc HFinal;
	HashUpdateXLProc HUpdateXL;
	int HAlgNum;
	THashName HName;
	int *HPtrOID;
	Word HLenOID;
	Byte HReserved[26];
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Byte MaxBlockLen = 0x80;
static const Shortint MaxDigestLen = 0x40;
static const Shortint MaxStateLen = 0x10;
static const Shortint MaxOIDLen = 0x9;
static const Word C_HashSig = 0x3d7a;
static const int C_HashVers = 0x10003;
#define C_MinHash (THashAlgorithm)(0)
#define C_MaxHash (THashAlgorithm)(8)
extern PACKAGE void __fastcall RegisterHash(THashAlgorithm AlgId, PHashDesc PHash);
extern PACKAGE PHashDesc __fastcall FindHash_by_ID(THashAlgorithm AlgoID);
extern PACKAGE PHashDesc __fastcall FindHash_by_Name( THashName &AlgoName);
extern PACKAGE void __fastcall HashUpdate(PHashDesc PHash, THashContext &Context, void * Msg, Word Len);
extern PACKAGE void __fastcall HashFullXL(PHashDesc PHash, Byte * Digest, void * Msg, int Len);
extern PACKAGE void __fastcall HashFull(PHashDesc PHash, Byte * Digest, void * Msg, Word Len);
extern PACKAGE void __fastcall HashFile(const System::ShortString &fname, PHashDesc PHash, Byte * Digest, void *buf, Word bsize, Word &Err);

}	/* namespace Zfhash */
using namespace Zfhash;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfhash
