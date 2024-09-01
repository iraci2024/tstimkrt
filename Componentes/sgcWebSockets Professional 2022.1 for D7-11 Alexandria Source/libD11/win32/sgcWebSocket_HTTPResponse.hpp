// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_HTTPResponse.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_httpresponseHPP
#define Sgcwebsocket_httpresponseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_httpresponse
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSHTTPResponse_Base;
class DELPHICLASS TsgcWSHTTPResponse;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TsgcWSHTTPResponse_BaseClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHTTPResponse_Base : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse() = 0 ;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSHTTPResponse_Base() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHTTPResponse_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHTTPResponse : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static System::Contnrs::TClassList* FServiceClassList;
	
protected:
	TsgcWSHTTPResponse_BaseClass __fastcall GetFileNameClass(const System::UnicodeString aServiceName);
	
public:
	__classmethod void __fastcall RegisterFileName(TsgcWSHTTPResponse_BaseClass aClass);
	System::UnicodeString __fastcall GetResponseMessage(const System::UnicodeString aFileName);
public:
	/* TObject.Create */ inline __fastcall TsgcWSHTTPResponse() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHTTPResponse() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_httpresponse */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_HTTPRESPONSE)
using namespace Sgcwebsocket_httpresponse;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_httpresponseHPP
