// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Extension_DeflateFrame.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_extension_deflateframeHPP
#define Sgcwebsocket_extension_deflateframeHPP

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

namespace Sgcwebsocket_extension_deflateframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMemStreamAccess;
class DELPHICLASS TsgcWSExtension_DeflateFrame;
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
class PASCALIMPLEMENTATION TsgcWSExtension_DeflateFrame : public Sgcwebsocket_extension_base::TsgcWSExtension_Base
{
	typedef Sgcwebsocket_extension_base::TsgcWSExtension_Base inherited;
	
public:
	virtual bool __fastcall DecodeExtension(const System::UnicodeString aExtension);
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	virtual void __fastcall DecodeHeader(System::Byte aByte);
	virtual void __fastcall EncodeHeader(System::Byte &aByte);
	virtual System::UnicodeString __fastcall GetName();
	
private:
	int FWindowBits;
	bool FMessageCompressed;
	void __fastcall SetWindowBits(const int Value);
	
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
	
protected:
	__property System::Classes::TStringStream* InfStream = {read=GetInfStream, write=FInfStream};
	int __fastcall IdRead(z_stream &aStrmRec, Idglobal::TIdBytes &VBuffer, int AOffset);
	int __fastcall DoRead(z_stream &aStrmRec, void *aBuffer);
	void __fastcall DoInflateFrame(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	
public:
	__fastcall virtual TsgcWSExtension_DeflateFrame();
	__fastcall virtual ~TsgcWSExtension_DeflateFrame();
	void __fastcall DeflateFrame(System::Classes::TStream* &aStream);
	void __fastcall InflateFrame(System::Classes::TStream* &aStream);
	__property bool MessageCompressed = {read=FMessageCompressed, nodefault};
	
__published:
	__property int WindowBits = {read=FWindowBits, write=SetWindowBits, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_extension_deflateframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_EXTENSION_DEFLATEFRAME)
using namespace Sgcwebsocket_extension_deflateframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_extension_deflateframeHPP
