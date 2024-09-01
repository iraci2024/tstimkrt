// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_apiHPP
#define Sgchttp_apiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcHTTP_Classes.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_api
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPAPI_client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcHTTPAPIExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

class PASCALIMPLEMENTATION TsgcHTTPAPI_client : public Sgchttp_classes::TsgcHTTPComponentClient_Base
{
	typedef Sgchttp_classes::TsgcHTTPComponentClient_Base inherited;
	
private:
	bool FLog;
	System::UnicodeString FLogFileName;
	Sgchttp_client::TsgcIdHTTPTLS_Options* FTLSOptions;
	Sgchttp_client::TsgcIdHTTPTLS_Options* __fastcall GetTLSOptions();
	
protected:
	__property bool Log = {read=FLog, write=FLog, nodefault};
	__property System::UnicodeString LogFileName = {read=FLogFileName, write=FLogFileName};
	
public:
	__property Sgchttp_client::TsgcIdHTTPTLS_Options* TLSOptions = {read=GetTLSOptions, write=FTLSOptions};
	__fastcall virtual TsgcHTTPAPI_client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTPAPI_client();
	
protected:
	virtual System::UnicodeString __fastcall DoGet(const System::UnicodeString aURL, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoPost(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoDelete(const System::UnicodeString aURL, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoPatch(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoPut(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	
public:
	System::UnicodeString __fastcall Get(const System::UnicodeString aURL, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall Post(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall Delete(const System::UnicodeString aURL, System::Classes::TStrings* const aHeader = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	System::UnicodeString __fastcall Patch(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall Put(const System::UnicodeString aURL, const System::UnicodeString aBody, System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	
private:
	TsgcHTTPAPIExceptionEvent FOnHTTPAPIException;
	
public:
	__property TsgcHTTPAPIExceptionEvent OnHTTPAPIException = {read=FOnHTTPAPIException, write=FOnHTTPAPIException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API)
using namespace Sgchttp_api;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_apiHPP
