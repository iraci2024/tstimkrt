// CodeGear C++Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Fxcpbkdf_2.pas' rev: 11.00

#ifndef Fxcpbkdf_2HPP
#define Fxcpbkdf_2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Fxckeyderiv.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcpbkdf_2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPBKDF2;
class PASCALIMPLEMENTATION TPBKDF2 : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FIterationNumber;
	Sysutils::TByteArray *FSaltValue;
	int FSaltLength;
	AnsiString FPassword;
	
public:
	__fastcall TPBKDF2(AnsiString Password, int SaltSize, int Iterations)/* overload */;
	__fastcall TPBKDF2(AnsiString Password, Sysutils::PByteArray Salt, int SaltLength, int Iterations)/* overload */;
	__fastcall virtual ~TPBKDF2(void);
	void __fastcall GetKeyBytes(int Cb, Sysutils::PByteArray &Key);
	void __fastcall Initialize(void);
	void __fastcall Reset(void);
	int __fastcall IterationCount(void);
	Sysutils::PByteArray __fastcall GetSaltValue(void);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Fxcpbkdf_2 */
using namespace Fxcpbkdf_2;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fxcpbkdf_2
