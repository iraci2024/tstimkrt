// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCCipher.pas' rev: 6.00

#ifndef FXCCipherHPP
#define FXCCipherHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <FXCDecUtil.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxccipher
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ECipherException;
class PASCALIMPLEMENTATION ECipherException : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	int ErrorCode;
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ECipherException(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ECipherException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ECipherException(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ECipherException(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ECipherException(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ECipherException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ECipherException(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ECipherException(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ECipherException(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TCipherMode { cmCTS, cmCBC, cmCFB, cmOFB, cmCTR, cmECB, cmCTSMAC, cmCBCMAC, cmCFBMAC };
#pragma option pop

typedef TMetaClass*TCipherClass;

class DELPHICLASS THash;
class PASCALIMPLEMENTATION THash : public Fxcdecutil::TProtection 
{
	typedef Fxcdecutil::TProtection inherited;
	
protected:
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall CodeInit(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Fxcdecutil::TPAction Action);
	void __fastcall Protect(bool IsInit);
	
public:
	__fastcall virtual ~THash(void);
	virtual void __fastcall Init(void);
	virtual void __fastcall Calc(const void *Data, int DataSize);
	virtual void __fastcall Done(void);
	virtual void * __fastcall DigestKey(void);
	AnsiString __fastcall DigestStr(int Format);
	/* virtual class method */ virtual int __fastcall DigestKeySize(TMetaClass* vmt);
	/*         class method */ static AnsiString __fastcall CalcBuffer(TMetaClass* vmt, const void *Buffer, int BufferSize, Fxcdecutil::TProtection* Protection = (Fxcdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	/*         class method */ static AnsiString __fastcall CalcStream(TMetaClass* vmt, const Classes::TStream* Stream, int StreamSize, Fxcdecutil::TProtection* Protection = (Fxcdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	/*         class method */ static AnsiString __fastcall CalcString(TMetaClass* vmt, const AnsiString Data, Fxcdecutil::TProtection* Protection = (Fxcdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	/*         class method */ static AnsiString __fastcall CalcFile(TMetaClass* vmt, const AnsiString FileName, Fxcdecutil::TProtection* Protection = (Fxcdecutil::TProtection*)(0x0), int Format = 0xffffffff)/* overload */;
	/*         class method */ static bool __fastcall SelfTest(TMetaClass* vmt);
public:
	#pragma option push -w-inl
	/* TProtection.Create */ inline __fastcall THash(Fxcdecutil::TProtection* AProtection) : Fxcdecutil::TProtection(AProtection) { }
	#pragma option pop
	
};


typedef TMetaClass*THashClass;

class DELPHICLASS TCipher;
class PASCALIMPLEMENTATION TCipher : public Fxcdecutil::TProtection 
{
	typedef Fxcdecutil::TProtection inherited;
	
private:
	TCipherMode FMode;
	THash* FHash;
	TMetaClass*FHashClass;
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
	void __fastcall SetHashClass(TMetaClass* Value);
	
protected:
	bool __fastcall GetFlag(int Index);
	virtual void __fastcall SetFlag(int Index, bool Value);
	void __fastcall InitBegin(int &Size);
	virtual void __fastcall InitEnd(void * IVector);
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall CodeInit(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeDone(Fxcdecutil::TPAction Action);
	virtual void __fastcall CodeBuf(void *Buffer, const int BufferSize, Fxcdecutil::TPAction Action);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	__property void * User = {read=FUser};
	__property void * Buffer = {read=FBuffer};
	__property int UserSize = {read=FUserSize, nodefault};
	
public:
	__fastcall TCipher(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */;
	__fastcall TCipher(Sysutils::PByteArray &key, Word keyLength)/* overload */;
	__fastcall virtual ~TCipher(void);
	/*         class method */ static int __fastcall MaxKeySize(TMetaClass* vmt);
	/*         class method */ static bool __fastcall SelfTest(TMetaClass* vmt);
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
	void __fastcall InitKey(const AnsiString Key, void * IVector)/* overload */;
	void __fastcall InitKey(Sysutils::PByteArray &Key, Word KeyLength, void * IVector)/* overload */;
	virtual void __fastcall Done(void);
	virtual void __fastcall Protect(void);
	void __fastcall EncodeBuffer(const void *Source, void *Dest, int DataSize);
	void __fastcall DecodeBuffer(const void *Source, void *Dest, int DataSize);
	__property TCipherMode Mode = {read=FMode, write=FMode, nodefault};
	__property THash* Hash = {read=GetHash};
	__property TMetaClass* HashClass = {read=FHashClass, write=SetHashClass};
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
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_Rijndael(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Rijndael(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_RijndaelFXC;
class PASCALIMPLEMENTATION TCipher_RijndaelFXC : public TCipher_Rijndael 
{
	typedef TCipher_Rijndael inherited;
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_RijndaelFXC(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher_Rijndael(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_RijndaelFXC(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_Blowfish;
class PASCALIMPLEMENTATION TCipher_Blowfish : public TCipher 
{
	typedef TCipher inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_Blowfish(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Blowfish(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_1DES;
class PASCALIMPLEMENTATION TCipher_1DES : public TCipher 
{
	typedef TCipher inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	void __fastcall MakeKey(const Byte * Data, const int Data_Size, void * Key_1, bool Reverse);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_1DES(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_1DES(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_3DES;
class PASCALIMPLEMENTATION TCipher_3DES : public TCipher_1DES 
{
	typedef TCipher_1DES inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_3DES(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher_1DES(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3DES(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_3TDES;
class PASCALIMPLEMENTATION TCipher_3TDES : public TCipher_3DES 
{
	typedef TCipher_3DES inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_3TDES(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher_3DES(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_3TDES(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_Twofish;
class PASCALIMPLEMENTATION TCipher_Twofish : public TCipher 
{
	typedef TCipher inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_Twofish(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Twofish(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCipher_Square;
class PASCALIMPLEMENTATION TCipher_Square : public TCipher 
{
	typedef TCipher inherited;
	
protected:
	/* virtual class method */ virtual void __fastcall GetContext(TMetaClass* vmt, int &ABufSize, int &AKeySize, int &AUserSize);
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Encode(void * Data);
	virtual void __fastcall Decode(void * Data);
	
public:
	virtual void __fastcall Init(const void *Key, int Size, void * IVector);
public:
	#pragma option push -w-inl
	/* TCipher.Create */ inline __fastcall TCipher_Square(const AnsiString Password, Fxcdecutil::TProtection* AProtection)/* overload */ : TCipher(Password, AProtection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCipher.Destroy */ inline __fastcall virtual ~TCipher_Square(void) { }
	#pragma option pop
	
};


class DELPHICLASS THash_MD4;
class PASCALIMPLEMENTATION THash_MD4 : public THash 
{
	typedef THash inherited;
	
private:
	unsigned FCount;
	Byte FBuffer[64];
	unsigned FDigest[10];
	
protected:
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Transform(Fxcdecutil::PIntArray Buffer);
	
public:
	/* virtual class method */ virtual int __fastcall DigestKeySize(TMetaClass* vmt);
	virtual void __fastcall Init(void);
	virtual void __fastcall Done(void);
	virtual void __fastcall Calc(const void *Data, int DataSize);
	virtual void * __fastcall DigestKey(void);
public:
	#pragma option push -w-inl
	/* THash.Destroy */ inline __fastcall virtual ~THash_MD4(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TProtection.Create */ inline __fastcall THash_MD4(Fxcdecutil::TProtection* AProtection) : THash(AProtection) { }
	#pragma option pop
	
};


class DELPHICLASS THash_RipeMD128;
class PASCALIMPLEMENTATION THash_RipeMD128 : public THash_MD4 
{
	typedef THash_MD4 inherited;
	
protected:
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Transform(Fxcdecutil::PIntArray Buffer);
public:
	#pragma option push -w-inl
	/* THash.Destroy */ inline __fastcall virtual ~THash_RipeMD128(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TProtection.Create */ inline __fastcall THash_RipeMD128(Fxcdecutil::TProtection* AProtection) : THash_MD4(AProtection) { }
	#pragma option pop
	
};


class DELPHICLASS THash_RipeMD256;
class PASCALIMPLEMENTATION THash_RipeMD256 : public THash_MD4 
{
	typedef THash_MD4 inherited;
	
protected:
	/* virtual class method */ virtual void * __fastcall TestVector(TMetaClass* vmt);
	virtual void __fastcall Transform(Fxcdecutil::PIntArray Buffer);
	
public:
	/* virtual class method */ virtual int __fastcall DigestKeySize(TMetaClass* vmt);
	virtual void __fastcall Init(void);
public:
	#pragma option push -w-inl
	/* THash.Destroy */ inline __fastcall virtual ~THash_RipeMD256(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TProtection.Create */ inline __fastcall THash_RipeMD256(Fxcdecutil::TProtection* AProtection) : THash_MD4(AProtection) { }
	#pragma option pop
	
};


class DELPHICLASS TChecksum;
class PASCALIMPLEMENTATION TChecksum : public THash 
{
	typedef THash inherited;
	
public:
	#pragma option push -w-inl
	/* THash.Destroy */ inline __fastcall virtual ~TChecksum(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TProtection.Create */ inline __fastcall TChecksum(Fxcdecutil::TProtection* AProtection) : THash(AProtection) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint Rijndael_Cipher = 0x1;
static const Shortint RIPEMD_128_Hash = 0x0;
static const Shortint RIPEMD_256_Hash = 0x1;
static const Shortint Default_Cipher_Method = 0x1;
static const Shortint Default_Hash_Method = 0x0;
static const Shortint errGeneric = 0x0;
static const Shortint errInvalidKey = 0x1;
static const Shortint errInvalidKeySize = 0x2;
static const Shortint errNotInitialized = 0x3;
static const Shortint errInvalidMACMode = 0x4;
static const Shortint errCantCalc = 0x5;
extern PACKAGE bool CheckCipherKeySize;
extern PACKAGE TMetaClass* __fastcall DefaultCipherClass(void);
extern PACKAGE void __fastcall SetDefaultCipherClass(TMetaClass* CipherClass);
extern PACKAGE void __fastcall RaiseCipherException(const int ErrorCode, const AnsiString Msg);
extern PACKAGE bool __fastcall RegisterCipher(const TMetaClass* ACipher, const AnsiString AName, const AnsiString ADescription);
extern PACKAGE bool __fastcall UnregisterCipher(const TMetaClass* ACipher);
extern PACKAGE Classes::TStrings* __fastcall CipherList(void);
extern PACKAGE void __fastcall CipherNames(Classes::TStrings* List);
extern PACKAGE TMetaClass* __fastcall GetCipherClass(const AnsiString Name);
extern PACKAGE AnsiString __fastcall GetCipherName(TMetaClass* CipherClass);
extern PACKAGE TMetaClass* __fastcall DefaultHashClass(void);
extern PACKAGE void __fastcall SetDefaultHashClass(TMetaClass* HashClass);
extern PACKAGE bool __fastcall RegisterHash(const TMetaClass* AHash, const AnsiString AName, const AnsiString ADescription);
extern PACKAGE bool __fastcall UnregisterHash(const TMetaClass* AHash);
extern PACKAGE Classes::TStrings* __fastcall HashList(void);
extern PACKAGE void __fastcall HashNames(Classes::TStrings* List);
extern PACKAGE TMetaClass* __fastcall GetHashClass(const AnsiString Name);
extern PACKAGE AnsiString __fastcall GetHashName(TMetaClass* HashClass);

}	/* namespace Fxccipher */
using namespace Fxccipher;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCCipher
