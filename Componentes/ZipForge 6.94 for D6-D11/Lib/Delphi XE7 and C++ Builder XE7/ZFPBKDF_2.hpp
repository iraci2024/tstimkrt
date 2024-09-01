// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFPBKDF_2.pas' rev: 28.00 (Windows)

#ifndef Zfpbkdf_2HPP
#define Zfpbkdf_2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <ZFKeyDeriv.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfpbkdf_2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPBKDF2;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPBKDF2 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FIterationNumber;
	System::Sysutils::TByteArray *FSaltValue;
	int FSaltLength;
	System::AnsiString FPassword;
	
public:
	__fastcall TPBKDF2(System::AnsiString Password, int SaltSize, int Iterations)/* overload */;
	__fastcall TPBKDF2(System::AnsiString Password, System::Sysutils::PByteArray Salt, int SaltLength, int Iterations)/* overload */;
	__fastcall virtual ~TPBKDF2(void);
	void __fastcall GetKeyBytes(int Cb, System::Sysutils::PByteArray &Key);
	void __fastcall Initialize(void);
	void __fastcall Reset(void);
	int __fastcall IterationCount(void);
	System::Sysutils::PByteArray __fastcall GetSaltValue(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Zfpbkdf_2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZFPBKDF_2)
using namespace Zfpbkdf_2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfpbkdf_2HPP
