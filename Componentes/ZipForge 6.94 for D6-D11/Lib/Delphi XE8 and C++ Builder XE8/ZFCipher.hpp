// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFCipher.pas' rev: 29.00 (Windows)

#ifndef ZfcipherHPP
#define ZfcipherHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ZFDecUtil.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zfcipher
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ECipherException;
class DELPHICLASS THash;
class DELPHICLASS TCipher;
class DELPHICLASS TCipher_Rijndael;
class DELPHICLASS TCipher_RijndaelZF;
class DELPHICLASS TCipher_Blowfish;
class DELPHICLASS TCipher_1DES;
class DELPHICLASS TCipher_3DES;
class DELPHICLASS TCipher_3TDES;
class DELPHICLASS TCipher_Twofish;
class DELPHICLASS TCipher_Square;
class DELPHICLASS THash_MD4;
class DELPHICLASS THash_RipeMD128;
class DELPHICLASS THash_RipeMD256;
class DELPHICLASS TChecksum;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION ECipherException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	int ErrorCode;
public:
	/* Exception.Create */ inline __fastcall ECipherException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECipherException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ECipherException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ECipherException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ECipherException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ECipherException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ECipherException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECipherException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECipherException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECipherException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECipherException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECipherException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECipherException(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TCipherMode : unsigned char { cmCTS, cmCBC, cmCFB, cmOFB, cmCTR, cmECB, cmCTSMAC, cmCBCMAC, cmCFBMAC };

typedef System::TMetaClass* TCipherClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash : public Zfdecutil::TProtection
{
	typedef Zfdecutil::TProtection inherited;
	
protected:
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall CodeInit(Zfdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Zfdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Zfdecutil::TPAction Action);
	void __fastcall Protect(bool IsInit);
	
public:
	__fastcall virtual ~THash(void);
	virtual void __fastcall Init(void);
	virtual void __fastcall Calc(const void *Data, int DataSize);
	virtual void __fastcall Done(void);
	virtual void * __fastcall DigestKey(void);
	System::AnsiString __fastcall DigestStr(int Format);
	__classmethod virtual int __fastcall DigestKeySize();
	__classmethod System::AnsiString __fastcall CalcBuffer(const void *Buffer, int BufferSize, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod System::AnsiString __fastcall CalcStream(System::Classes::TStream* const Stream, int StreamSize, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod System::AnsiString __fastcall CalcString(const System::AnsiString Data, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod System::AnsiString __fastcall CalcFile(const System::AnsiString FileName, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod bool __fastcall SelfTest();
public:
	/* TProtection.Create */ inline __fastcall THash(Zfdecutil::TProtection* AProtection) : Zfdecutil::TProtection(AProtection) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* THashClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher : public Zfdecutil::TProtection
{
	typedef Zfdecutil::TProtection inherited;
	
private:
	TCipherMode FMode;
	THash* FHash;
	THashClass FHashClass;
	int FKeySize;
	int FBufSize;
	int FUserSize;
	void *FBuffer;
	void *FVector;
	void *FFeedback;
	void *FUser;
	int FFlags;
	int ctrMode_Position;
	void *ctrMode_EncryptionBlock;
	void *ctrMode_Nonce;
	THash* __fastcall GetHash(void);
	void __fastcall SetHashClass(THashClass Value);
	
protected:
	bool __fastcall GetFlag(int Index);
	virtual void __fastcall SetFlag(int Index, bool Value);
	void __fastcall InitBegin(int &Size);
	virtual void __fastcall InitEnd(void * IVector);
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall CodeInit(Zfdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Zfdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Zfdecutil::TPAction Action);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	__property void * User = {read=FUser};
	__property void * Buffer = {read=FBuffer};
	__property int UserSize = {read=FUserSize, nodefault};
	
public:
	__fastcall TCipher(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */;
	__fastcall TCipher(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */;
	__fastcall virtual ~TCipher(void);
	__classmethod int __fastcall MaxKeySize();
	__classmethod bool __fastcall SelfTest();
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
	void __fastcall InitKey(const System::AnsiString Key, void * IVector)/* overload */;
	void __fastcall InitKey(System::Sysutils::PByteArray &Key, System::Word KeyLength, void * IVector)/* overload */;
	virtual void __fastcall Done(void);
	virtual void __fastcall Protect(void);
	void __fastcall EncodeBuffer(const void *Source, void *Dest, int DataSize);
	void __fastcall DecodeBuffer(const void *Source, void *Dest, int DataSize);
	__property TCipherMode Mode = {read=FMode, write=FMode, nodefault};
	__property THash* Hash = {read=GetHash};
	__property THashClass HashClass = {read=FHashClass, write=SetHashClass};
	__property int KeySize = {read=FKeySize, nodefault};
	__property int BufSize = {read=FBufSize, nodefault};
	__property bool Initialized = {read=GetFlag, write=SetFlag, index=1, nodefault};
	__property void * Vector = {read=FVector};
	__property void * Feedback = {read=FFeedback};
	__property bool HasHashKey = {read=GetFlag, index=0, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_Rijndael : public TCipher
{
	typedef TCipher inherited;
	
private:
	int FRounds;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_Rijndael(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_Rijndael(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Rijndael(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_RijndaelZF : public TCipher_Rijndael
{
	typedef TCipher_Rijndael inherited;
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_RijndaelZF(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher_Rijndael(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_RijndaelZF(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher_Rijndael(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_RijndaelZF(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_Blowfish : public TCipher
{
	typedef TCipher inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_Blowfish(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_Blowfish(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Blowfish(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_1DES : public TCipher
{
	typedef TCipher inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	void __fastcall MakeKey(System::Byte const *Data, const int Data_High, void * Key_1, bool Reverse);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_1DES(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_1DES(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_1DES(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_3DES : public TCipher_1DES
{
	typedef TCipher_1DES inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_3DES(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher_1DES(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_3DES(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher_1DES(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3DES(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_3TDES : public TCipher_3DES
{
	typedef TCipher_3DES inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
public:
	/* TCipher.Create */ inline __fastcall TCipher_3TDES(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher_3DES(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_3TDES(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher_3DES(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3TDES(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_Twofish : public TCipher
{
	typedef TCipher inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_Twofish(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_Twofish(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Twofish(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCipher_Square : public TCipher
{
	typedef TCipher inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_Square(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Create */ inline __fastcall TCipher_Square(System::Sysutils::PByteArray &key, System::Word keyLength)/* overload */ : TCipher(key, keyLength) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Square(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_MD4 : public THash
{
	typedef THash inherited;
	
private:
	unsigned FCount;
	System::StaticArray<System::Byte, 64> FBuffer;
	System::StaticArray<unsigned, 10> FDigest;
	
protected:
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Transform(Zfdecutil::PIntArray Buffer);
	
public:
	__classmethod virtual int __fastcall DigestKeySize();
	virtual void __fastcall Init(void);
	virtual void __fastcall Done(void);
	virtual void __fastcall Calc(const void *Data, int DataSize);
	virtual void * __fastcall DigestKey(void);
public:
	/* THash.Destroy */ inline __fastcall virtual ~THash_MD4(void) { }
	
public:
	/* TProtection.Create */ inline __fastcall THash_MD4(Zfdecutil::TProtection* AProtection) : THash(AProtection) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_RipeMD128 : public THash_MD4
{
	typedef THash_MD4 inherited;
	
protected:
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Transform(Zfdecutil::PIntArray Buffer);
public:
	/* THash.Destroy */ inline __fastcall virtual ~THash_RipeMD128(void) { }
	
public:
	/* TProtection.Create */ inline __fastcall THash_RipeMD128(Zfdecutil::TProtection* AProtection) : THash_MD4(AProtection) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_RipeMD256 : public THash_MD4
{
	typedef THash_MD4 inherited;
	
protected:
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Transform(Zfdecutil::PIntArray Buffer);
	
public:
	__classmethod virtual int __fastcall DigestKeySize();
	virtual void __fastcall Init(void);
public:
	/* THash.Destroy */ inline __fastcall virtual ~THash_RipeMD256(void) { }
	
public:
	/* TProtection.Create */ inline __fastcall THash_RipeMD256(Zfdecutil::TProtection* AProtection) : THash_MD4(AProtection) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TChecksum : public THash
{
	typedef THash inherited;
	
public:
	/* THash.Destroy */ inline __fastcall virtual ~TChecksum(void) { }
	
public:
	/* TProtection.Create */ inline __fastcall TChecksum(Zfdecutil::TProtection* AProtection) : THash(AProtection) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 Rijndael_Cipher = System::Int8(0x1);
static const System::Int8 RIPEMD_128_Hash = System::Int8(0x0);
static const System::Int8 RIPEMD_256_Hash = System::Int8(0x1);
static const System::Int8 Default_Cipher_Method = System::Int8(0x1);
static const System::Int8 Default_Hash_Method = System::Int8(0x0);
static const System::Int8 errGeneric = System::Int8(0x0);
static const System::Int8 errInvalidKey = System::Int8(0x1);
static const System::Int8 errInvalidKeySize = System::Int8(0x2);
static const System::Int8 errNotInitialized = System::Int8(0x3);
static const System::Int8 errInvalidMACMode = System::Int8(0x4);
static const System::Int8 errCantCalc = System::Int8(0x5);
extern DELPHI_PACKAGE bool CheckCipherKeySize;
extern DELPHI_PACKAGE TCipherClass __fastcall DefaultCipherClass(void);
extern DELPHI_PACKAGE void __fastcall SetDefaultCipherClass(TCipherClass CipherClass);
extern DELPHI_PACKAGE void __fastcall RaiseCipherException(const int ErrorCode, const System::UnicodeString Msg);
extern DELPHI_PACKAGE bool __fastcall RegisterCipher(const TCipherClass ACipher, const System::UnicodeString AName, const System::UnicodeString ADescription);
extern DELPHI_PACKAGE bool __fastcall UnregisterCipher(const TCipherClass ACipher);
extern DELPHI_PACKAGE System::Classes::TStrings* __fastcall CipherList(void);
extern DELPHI_PACKAGE void __fastcall CipherNames(System::Classes::TStrings* List);
extern DELPHI_PACKAGE TCipherClass __fastcall GetCipherClass(const System::UnicodeString Name);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCipherName(TCipherClass CipherClass);
extern DELPHI_PACKAGE THashClass __fastcall DefaultHashClass(void);
extern DELPHI_PACKAGE void __fastcall SetDefaultHashClass(THashClass HashClass);
extern DELPHI_PACKAGE bool __fastcall RegisterHash(const THashClass AHash, const System::UnicodeString AName, const System::UnicodeString ADescription);
extern DELPHI_PACKAGE bool __fastcall UnregisterHash(const THashClass AHash);
extern DELPHI_PACKAGE System::Classes::TStrings* __fastcall HashList(void);
extern DELPHI_PACKAGE void __fastcall HashNames(System::Classes::TStrings* List);
extern DELPHI_PACKAGE THashClass __fastcall GetHashClass(const System::UnicodeString Name);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetHashName(THashClass HashClass);
}	/* namespace Zfcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFCIPHER)
using namespace Zfcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfcipherHPP
