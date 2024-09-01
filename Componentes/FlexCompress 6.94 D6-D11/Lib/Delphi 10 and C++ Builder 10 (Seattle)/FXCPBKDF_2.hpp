// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCPBKDF_2.pas' rev: 30.00 (Windows)

#ifndef Fxcpbkdf_2HPP
#define Fxcpbkdf_2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <FXCKeyDeriv.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxcpbkdf_2
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPBKDF2;
//-- type declarations -------------------------------------------------------
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
}	/* namespace Fxcpbkdf_2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCPBKDF_2)
using namespace Fxcpbkdf_2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcpbkdf_2HPP
