// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Helpers.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_helpersHPP
#define Sgcwebsocket_helpersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.Masks.hpp>
#include <IdGlobal.hpp>
#include <IdCoderMIME.hpp>
#include <IdHeaderList.hpp>
#include <IdSync.hpp>
#include <IdURI.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_helpers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcDelimitedStringList;
class DELPHICLASS TsgcFileStream;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Byte> TsgcArrayOfBytes;

class PASCALIMPLEMENTATION TsgcDelimitedStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	__fastcall TsgcDelimitedStringList();
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TsgcDelimitedStringList() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcFileStream : public System::Classes::TFileStream
{
	typedef System::Classes::TFileStream inherited;
	
public:
	/* TFileStream.Create */ inline __fastcall TsgcFileStream(const System::UnicodeString AFileName, System::Word Mode)/* overload */ : System::Classes::TFileStream(AFileName, Mode) { }
	/* TFileStream.Create */ inline __fastcall TsgcFileStream(const System::UnicodeString AFileName, System::Word Mode, unsigned Rights)/* overload */ : System::Classes::TFileStream(AFileName, Mode, Rights) { }
	/* TFileStream.Destroy */ inline __fastcall virtual ~TsgcFileStream() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall HexToBase64(System::UnicodeString aText);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcBytesToString(Sgcbase_helpers::TsgcStringStream* aStream);
extern DELPHI_PACKAGE Idglobal::TIdBytes __fastcall sgcStringToBytes(const System::UnicodeString aText);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeGETHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeGETFileName(System::UnicodeString aText);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeGETFullPath(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsWebSocketHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE void __fastcall ResizeStream(System::Classes::TStream* aStream, int aSize);
extern DELPHI_PACKAGE bool __fastcall IsAuthenticationHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsWebsocketAuthenticationHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsRequestAuthenticationSessionHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsAuthenticationURLHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsAuthenticationSessionHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall sgcMatchesMask(const System::UnicodeString FileName, const System::UnicodeString Mask);
extern DELPHI_PACKAGE void __fastcall sgcFree(System::TObject* const &Obj);
extern DELPHI_PACKAGE bool __fastcall IsAuthenticationBasicHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeAuthorizationBasic(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeGETProtocol(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsRequestFlashPolicy(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall IsSSEHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE void __fastcall sgcWSStreamWrite(const System::UnicodeString aHeader, System::Classes::TMemoryStream* &Stream);
extern DELPHI_PACKAGE void __fastcall sgcWSStreamRead(System::Classes::TMemoryStream* &Stream, System::UnicodeString &Header);
extern DELPHI_PACKAGE bool __fastcall IsXHRHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodePOSTHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodePOSTFullPath(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::Classes::TFileStream* __fastcall sgcWSStreamWriteFile(const System::UnicodeString aHeader, System::Classes::TFileStream* const Stream);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTEMPDirectory(void);
extern DELPHI_PACKAGE bool __fastcall DecodeWSSURL(const System::UnicodeString aText, System::UnicodeString &Host, int &Port, bool &SSL);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetEncodedString(const System::UnicodeString aText);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetDecodedString(const System::UnicodeString aText);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcArrayOfBytesToString(System::DynamicArray<System::Byte> aArray);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall StringTosgcArrayOfBytes(System::UnicodeString aString);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcArrayOfBytesToStringWithoutNull(System::DynamicArray<System::Byte> aArray);
extern DELPHI_PACKAGE void __fastcall NotifyMethod(System::Classes::TThreadMethod aMethod);
extern DELPHI_PACKAGE void __fastcall SynchronizeMethod(System::Classes::TThreadMethod aMethod);
extern DELPHI_PACKAGE void __fastcall ParseURL(const System::UnicodeString aURL, /* out */ System::UnicodeString &Protocol, /* out */ System::UnicodeString &Host, /* out */ System::UnicodeString &Port, /* out */ System::UnicodeString &Path, /* out */ System::UnicodeString &Query);
extern DELPHI_PACKAGE void __fastcall sgcAddBytes(Idglobal::TIdBytes aBytesOrg, Idglobal::TIdBytes &aBytesDest);
extern DELPHI_PACKAGE __int64 __fastcall DateToMilliseconds(System::TDateTime aDate);
extern DELPHI_PACKAGE bool __fastcall IsHTTPHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcFetch(System::UnicodeString &aText, const System::UnicodeString aDelim, const bool aDelete = true);
extern DELPHI_PACKAGE bool __fastcall IsHTTPProtocolHeader(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE bool __fastcall sgcStartsText(const System::UnicodeString aText, const System::UnicodeString aSubText);
extern DELPHI_PACKAGE bool __fastcall IsHTTP2Header(System::UnicodeString aValue);
extern DELPHI_PACKAGE bool __fastcall IsWatchDogConnection(System::Classes::TStringList* aHeaders, System::UnicodeString aSecret);
extern DELPHI_PACKAGE bool __fastcall IsOAuth2Header(System::Classes::TStringList* aHeaders, System::UnicodeString aAuthURL, System::UnicodeString aMethod = L"GET");
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeAuthorizationBearer(System::Classes::TStringList* aHeaders);
extern DELPHI_PACKAGE __int64 __fastcall sgcGetContentLength(System::Classes::TStringList* const aHeaders)/* overload */;
extern DELPHI_PACKAGE __int64 __fastcall sgcGetContentLength(const System::UnicodeString aHeader)/* overload */;
extern DELPHI_PACKAGE __int64 __fastcall sgcGetContentLength(Idheaderlist::TIdHeaderList* const aHeaders)/* overload */;
}	/* namespace Sgcwebsocket_helpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_HELPERS)
using namespace Sgcwebsocket_helpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_helpersHPP
