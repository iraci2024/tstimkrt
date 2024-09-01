// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFPBKDF_2.pas' rev: 4.00

#ifndef ZFPBKDF_2HPP
#define ZFPBKDF_2HPP

#pragma delphiheader begin
#pragma option push -w-
#include <ZFKeyDeriv.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfpbkdf_2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPBKDF2;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPBKDF2 : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FIterationNumber;
	Byte *FSaltValue;
	int FSaltLength;
	AnsiString FPassword;
	
public:
	__fastcall TPBKDF2(AnsiString Password, int SaltSize, int Iterations)/* overload */;
	__fastcall TPBKDF2(AnsiString Password, Sysutils::PByteArray Salt, int SaltLength, int Iterations)/* overload */
		;
	__fastcall virtual ~TPBKDF2(void);
	void __fastcall GetKeyBytes(int Cb, Sysutils::PByteArray &Key);
	void __fastcall Initialize(void);
	void __fastcall Reset(void);
	int __fastcall IterationCount(void);
	Sysutils::PByteArray __fastcall GetSaltValue(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Zfpbkdf_2 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfpbkdf_2;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFPBKDF_2
