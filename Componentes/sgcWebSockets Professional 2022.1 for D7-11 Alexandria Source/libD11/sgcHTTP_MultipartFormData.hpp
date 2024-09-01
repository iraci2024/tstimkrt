// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_MultipartFormData.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_multipartformdataHPP
#define Sgchttp_multipartformdataHPP

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
#include <sgcBase_Helpers.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_multipartformdata
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPMultipartFormDataDecoder;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcHTTPMultipartFormDataBeforeSaveFileEvent)(System::TObject* Sender, System::UnicodeString &aFileName, System::UnicodeString &aFilePath);

typedef void __fastcall (__closure *TsgcHTTPMultipartFormDataAfterSaveFileEvent)(System::TObject* Sender, const System::UnicodeString aFileName, const System::UnicodeString aFilePath);

class PASCALIMPLEMENTATION TsgcHTTPMultipartFormDataDecoder : public Sgchttp_classes::TsgcHTTPComponent_Base
{
	typedef Sgchttp_classes::TsgcHTTPComponent_Base inherited;
	
private:
	int FReadBlockSize;
	bool __fastcall SameBytes(const System::DynamicArray<System::Byte> aBytes1, const System::DynamicArray<System::Byte> aBytes2, __int64 &aPosition);
	
protected:
	System::UnicodeString __fastcall GetFileName(const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcHTTPMultipartFormDataDecoder(System::Classes::TComponent* aOwner);
	void __fastcall ExtractFiles(System::Classes::TStream* const aStream, const System::UnicodeString aBoundary, const System::UnicodeString aPath = System::UnicodeString());
	__property int ReadBlockSize = {read=FReadBlockSize, write=FReadBlockSize, nodefault};
	
private:
	TsgcHTTPMultipartFormDataBeforeSaveFileEvent FOnBeforeSaveFile;
	TsgcHTTPMultipartFormDataAfterSaveFileEvent FOnAfterSaveFile;
	
public:
	__property TsgcHTTPMultipartFormDataBeforeSaveFileEvent OnBeforeSaveFile = {read=FOnBeforeSaveFile, write=FOnBeforeSaveFile};
	__property TsgcHTTPMultipartFormDataAfterSaveFileEvent OnAfterSaveFile = {read=FOnAfterSaveFile, write=FOnAfterSaveFile};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPMultipartFormDataDecoder() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_multipartformdata */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_MULTIPARTFORMDATA)
using namespace Sgchttp_multipartformdata;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_multipartformdataHPP
