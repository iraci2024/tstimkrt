// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Extension_PerMessage_Deflate.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_extension_permessage_deflateHPP
#define Sgcwebsocket_extension_permessage_deflateHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdZLib.hpp>
#include <IdGlobal.hpp>
#include <IdStream.hpp>
#include <IdZLibHeaders.hpp>
#include <IdCTypes.hpp>
#include <sgcWebSocket_Extension_Base.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_extension_permessage_deflate
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMemStreamAccess;
class DELPHICLASS TsgcWSExtension_PerMessage_Deflate;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMemStreamAccess : public System::Classes::TMemoryStream
{
	typedef System::Classes::TMemoryStream inherited;
	
public:
	/* TMemoryStream.Destroy */ inline __fastcall virtual ~TMemStreamAccess() { }
	
public:
	/* TObject.Create */ inline __fastcall TMemStreamAccess() : System::Classes::TMemoryStream() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSExtension_PerMessage_Deflate : public Sgcwebsocket_extension_base::TsgcWSExtension_Base
{
	typedef Sgcwebsocket_extension_base::TsgcWSExtension_Base inherited;
	
public:
	virtual bool __fastcall DecodeExtension(const System::UnicodeString aExtension);
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	virtual void __fastcall DecodeHeader(System::Byte aByte);
	virtual void __fastcall EncodeHeader(System::Byte &aByte);
	virtual System::UnicodeString __fastcall GetHeaderExtension();
	virtual System::UnicodeString __fastcall GetName();
	
private:
	bool FMessageCompressed;
	
protected:
	bool __fastcall ExpandStream(System::Classes::TStream* AStream, const __int64 ACapacity);
	void * __fastcall DMAOfStream(System::Classes::TStream* AStream, /* out */ unsigned &Available);
	bool __fastcall CanResizeDMAStream(System::Classes::TStream* AStream);
	
private:
	System::Classes::TStringStream* FDefStream;
	System::Classes::TStringStream* __fastcall GetDefStream();
	
protected:
	__property System::Classes::TStringStream* DefStream = {read=GetDefStream, write=FDefStream};
	void __fastcall DoDeflateFrame(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	
private:
	Idglobal::TIdBytes FReadBytes;
	System::Classes::TStringStream* FInfStream;
	void __fastcall DoInitializeInflateBuffer();
	System::Classes::TStringStream* __fastcall GetInfStream();
	void __fastcall SetClientMaxWindowBits(const int Value);
	void __fastcall SetClientNoContextTakeOver(const bool Value);
	void __fastcall SetServerMaxWindowBits(const int Value);
	void __fastcall SetServerNoContextTakeOver(const bool Value);
	
protected:
	__property System::Classes::TStringStream* InfStream = {read=GetInfStream, write=FInfStream};
	void __fastcall DoInflateFrame(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	
public:
	__fastcall virtual TsgcWSExtension_PerMessage_Deflate();
	__fastcall virtual ~TsgcWSExtension_PerMessage_Deflate();
	void __fastcall DeflateFrame(System::Classes::TStream* &aStream);
	void __fastcall InflateFrame(System::Classes::TStream* &aStream);
	__property bool MessageCompressed = {read=FMessageCompressed, nodefault};
	
private:
	int FServerMaxWindowBits;
	bool FServerNoContextTakeOver;
	int FClientMaxWindowBits;
	bool FClientNoContextTakeOver;
	int FMemLevel;
	
__published:
	__property int ClientMaxWindowBits = {read=FClientMaxWindowBits, write=SetClientMaxWindowBits, nodefault};
	__property bool ClientNoContextTakeOver = {read=FClientNoContextTakeOver, write=SetClientNoContextTakeOver, nodefault};
	__property int MemLevel = {read=FMemLevel, write=FMemLevel, nodefault};
	__property int ServerMaxWindowBits = {read=FServerMaxWindowBits, write=SetServerMaxWindowBits, nodefault};
	__property bool ServerNoContextTakeOver = {read=FServerNoContextTakeOver, write=SetServerNoContextTakeOver, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_extension_permessage_deflate */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_EXTENSION_PERMESSAGE_DEFLATE)
using namespace Sgcwebsocket_extension_permessage_deflate;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_extension_permessage_deflateHPP
