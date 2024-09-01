// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfcipher.pas' rev: 20.00

#ifndef ZfcipherHPP
#define ZfcipherHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Zfdecutil.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfcipher
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ECipherException;
class PASCALIMPLEMENTATION ECipherException : public Sysutils::Exception
{
	typedef Sysutils::Exception inherited;
	
public:
	int ErrorCode;
public:
	/* Exception.Create */ inline __fastcall ECipherException(const System::UnicodeString Msg) : Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECipherException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ECipherException(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	/* Exception.CreateResFmt */ inline __fastcall ECipherException(int Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ECipherException(const System::UnicodeString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECipherException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECipherException(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECipherException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECipherException(void) { }
	
};


#pragma option push -b-
enum TCipherMode { cmCTS, cmCBC, cmCFB, cmOFB, cmCTR, cmECB, cmCTSMAC, cmCBCMAC, cmCFBMAC };
#pragma option pop

typedef TMetaClass* TCipherClass;

class DELPHICLASS THash;
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
	__classmethod System::AnsiString __fastcall CalcStream(const Classes::TStream* Stream, int StreamSize, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod System::AnsiString __fastcall CalcString(const System::AnsiString Data, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod System::AnsiString __fastcall CalcFile(const System::AnsiString FileName, Zfdecutil::TProtection* Protection = (Zfdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	__classmethod bool __fastcall SelfTest();
public:
	/* TProtection.Create */ inline __fastcall THash(Zfdecutil::TProtection* AProtection) : Zfdecutil::TProtection(AProtection) { }
	
};


typedef TMetaClass* THashClass;

class DELPHICLASS TCipher;
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
	__fastcall TCipher(Sysutils::PByteArray &key, System::Word keyLength)/* overload */;
	__fastcall virtual ~TCipher(void);
	__classmethod int __fastcall MaxKeySize();
	__classmethod bool __fastcall SelfTest();
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
	void __fastcall InitKey(const System::AnsiString Key, void * IVector)/* overload */;
	void __fastcall InitKey(Sysutils::PByteArray &Key, System::Word KeyLength, void * IVector)/* overload */;
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


class DELPHICLASS TCipher_Rijndael;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Rijndael(void) { }
	
};


class DELPHICLASS TCipher_RijndaelZF;
class PASCALIMPLEMENTATION TCipher_RijndaelZF : public TCipher_Rijndael
{
	typedef TCipher_Rijndael inherited;
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_RijndaelZF(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher_Rijndael(Password, AProtection) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_RijndaelZF(void) { }
	
};


class DELPHICLASS TCipher_Blowfish;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Blowfish(void) { }
	
};


class DELPHICLASS TCipher_1DES;
class PASCALIMPLEMENTATION TCipher_1DES : public TCipher
{
	typedef TCipher inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &ABufSize, int &AKeySize, int &AUserSize);
	__classmethod virtual void * __fastcall TestVector();
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	void __fastcall MakeKey(System::Byte const *Data, const int Data_Size, void * Key_1, bool Reverse);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	/* TCipher.Create */ inline __fastcall TCipher_1DES(const System::AnsiString Password, Zfdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_1DES(void) { }
	
};


class DELPHICLASS TCipher_3DES;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3DES(void) { }
	
};


class DELPHICLASS TCipher_3TDES;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3TDES(void) { }
	
};


class DELPHICLASS TCipher_Twofish;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Twofish(void) { }
	
};


class DELPHICLASS TCipher_Square;
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
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Square(void) { }
	
};


class DELPHICLASS THash_MD4;
class PASCALIMPLEMENTATION THash_MD4 : public THash
{
	typedef THash inherited;
	
private:
	unsigned FCount;
	StaticArray<System::Byte, 64> FBuffer;
	StaticArray<unsigned, 10> FDigest;
	
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


class DELPHICLASS THash_RipeMD128;
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


class DELPHICLASS THash_RipeMD256;
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


class DELPHICLASS TChecksum;
class PASCALIMPLEMENTATION TChecksum : public THash
{
	typedef THash inherited;
	
public:
	/* THash.Destroy */ inline __fastcall virtual ~TChecksum(void) { }
	
public:
	/* TProtection.Create */ inline __fastcall TChecksum(Zfdecutil::TProtection* AProtection) : THash(AProtection) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const ShortInt Rijndael_Cipher = 0x1;
static const ShortInt RIPEMD_128_Hash = 0x0;
static const ShortInt RIPEMD_256_Hash = 0x1;
static const ShortInt Default_Cipher_Method = 0x1;
static const ShortInt Default_Hash_Method = 0x0;
static const ShortInt errGeneric = 0x0;
static const ShortInt errInvalidKey = 0x1;
static const ShortInt errInvalidKeySize = 0x2;
static const ShortInt errNotInitialized = 0x3;
static const ShortInt errInvalidMACMode = 0x4;
static const ShortInt errCantCalc = 0x5;
extern PACKAGE bool CheckCipherKeySize;
extern PACKAGE TCipherClass __fastcall DefaultCipherClass(void);
extern PACKAGE void __fastcall SetDefaultCipherClass(TCipherClass CipherClass);
extern PACKAGE void __fastcall RaiseCipherException(const int ErrorCode, const System::UnicodeString Msg);
extern PACKAGE bool __fastcall RegisterCipher(const TCipherClass ACipher, const System::UnicodeString AName, const System::UnicodeString ADescription);
extern PACKAGE bool __fastcall UnregisterCipher(const TCipherClass ACipher);
extern PACKAGE Classes::TStrings* __fastcall CipherList(void);
extern PACKAGE void __fastcall CipherNames(Classes::TStrings* List);
extern PACKAGE TCipherClass __fastcall GetCipherClass(const System::UnicodeString Name);
extern PACKAGE System::UnicodeString __fastcall GetCipherName(TCipherClass CipherClass);
extern PACKAGE THashClass __fastcall DefaultHashClass(void);
extern PACKAGE void __fastcall SetDefaultHashClass(THashClass HashClass);
extern PACKAGE bool __fastcall RegisterHash(const THashClass AHash, const System::UnicodeString AName, const System::UnicodeString ADescription);
extern PACKAGE bool __fastcall UnregisterHash(const THashClass AHash);
extern PACKAGE Classes::TStrings* __fastcall HashList(void);
extern PACKAGE void __fastcall HashNames(Classes::TStrings* List);
extern PACKAGE THashClass __fastcall GetHashClass(const System::UnicodeString Name);
extern PACKAGE System::UnicodeString __fastcall GetHashName(THashClass HashClass);

}	/* namespace Zfcipher */
using namespace Zfcipher;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfcipherHPP
