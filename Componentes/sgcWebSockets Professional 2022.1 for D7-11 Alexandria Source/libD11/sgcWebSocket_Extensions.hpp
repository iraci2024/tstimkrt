// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Extensions.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_extensionsHPP
#define Sgcwebsocket_extensionsHPP

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
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Extension_DeflateFrame.hpp>
#include <sgcWebSocket_Extension_PerMessage_Deflate.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_extensions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSExtensions;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSExtensions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FExtensionNegotiated;
	Sgcwebsocket_extension_deflateframe::TsgcWSExtension_DeflateFrame* FDeflateFrame;
	System::Classes::TStringList* FList;
	Sgcwebsocket_types::TwsApplicationMode FMode;
	Sgcwebsocket_extension_permessage_deflate::TsgcWSExtension_PerMessage_Deflate* FPerMessage_Deflate;
	bool __fastcall GetCompression();
	System::Classes::TStringList* __fastcall GetList();
	void __fastcall SetMode(const Sgcwebsocket_types::TwsApplicationMode Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__fastcall virtual TsgcWSExtensions();
	__fastcall virtual ~TsgcWSExtensions();
	void __fastcall DecodeHeader(System::Byte aByte);
	void __fastcall EncodeHeader(System::Byte &aByte);
	void __fastcall DecodeExtensions(const System::UnicodeString aExtensions);
	void __fastcall WriteHeader(System::Classes::TStringList* aHeader);
	void __fastcall ReadStream(System::Classes::TStream* &aStream);
	void __fastcall WriteStream(System::Classes::TStream* &aStream);
	__property System::Classes::TStringList* List = {read=GetList};
	__property bool ExtensionNegotiated = {read=FExtensionNegotiated, nodefault};
	__property bool Compression = {read=GetCompression, nodefault};
	__property Sgcwebsocket_types::TwsApplicationMode Mode = {read=FMode, write=SetMode, nodefault};
	
__published:
	__property Sgcwebsocket_extension_deflateframe::TsgcWSExtension_DeflateFrame* DeflateFrame = {read=FDeflateFrame, write=FDeflateFrame};
	__property Sgcwebsocket_extension_permessage_deflate::TsgcWSExtension_PerMessage_Deflate* PerMessage_Deflate = {read=FPerMessage_Deflate, write=FPerMessage_Deflate};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_extensions */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_EXTENSIONS)
using namespace Sgcwebsocket_extensions;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_extensionsHPP
