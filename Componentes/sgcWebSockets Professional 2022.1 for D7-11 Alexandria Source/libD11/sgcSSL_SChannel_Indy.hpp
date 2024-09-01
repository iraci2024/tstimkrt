// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcSSL_SChannel_Indy.pas' rev: 35.00 (Windows)

#ifndef Sgcssl_schannel_indyHPP
#define Sgcssl_schannel_indyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdGlobal.hpp>
#include <IdSSL.hpp>
#include <IdURI.hpp>
#include <IdCustomTransparentProxy.hpp>
#include <sgcSSL_SChannel.hpp>
#include <sgcWebSocket_Types.hpp>
#include <IdIOHandlerStack.hpp>
#include <IdIOHandlerSocket.hpp>
#include <IdIOHandler.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcssl_schannel_indy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS SChannelError;
class DELPHICLASS TsgcIdSSLIOHandlerSocketSChannel;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION SChannelError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	unsigned CertStatus;
	__fastcall SChannelError(System::UnicodeString AMsg, unsigned AStatus);
public:
	/* Exception.CreateFmt */ inline __fastcall SChannelError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall SChannelError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall SChannelError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall SChannelError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall SChannelError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall SChannelError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall SChannelError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall SChannelError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall SChannelError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall SChannelError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall SChannelError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~SChannelError() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCredentialsEvent)(System::TObject* Sender);

class PASCALIMPLEMENTATION TsgcIdSSLIOHandlerSocketSChannel : public Idssl::TIdSSLIOHandlerSocketBase
{
	typedef Idssl::TIdSSLIOHandlerSocketBase inherited;
	
private:
	System::Classes::TStringList* FALPNProtocols;
	NativeUInt FSSL;
	TCredentialsEvent FOnCredentials;
	Sgcssl_schannel::TsgcSChannelCertificate* FCertificate;
	System::UnicodeString FCipherList;
	Sgcwebsocket_types::TwsTLSVersions FTLSVersion;
	void __fastcall SetCredentials(TCredentialsEvent Value);
	void __fastcall ConnectSSL();
	void __fastcall CloseSSL();
	System::Classes::TStringList* __fastcall GetALPNProtocols();
	System::UnicodeString __fastcall GetTargetHost();
	void __fastcall SetALPNProtocols(System::Classes::TStringList* const Value);
	void __fastcall SetCertificate(Sgcssl_schannel::TsgcSChannelCertificate* const Value);
	
protected:
	virtual void __fastcall SetPassThrough(const bool Value);
	virtual int __fastcall RecvEnc(Idglobal::TIdBytes &ABuffer);
	virtual int __fastcall SendEnc(const Idglobal::TIdBytes ABuffer, const int AOffset, const int ALength);
	virtual void __fastcall ConnectClient();
	
public:
	__fastcall virtual TsgcIdSSLIOHandlerSocketSChannel(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcIdSSLIOHandlerSocketSChannel();
	virtual Idssl::TIdSSLIOHandlerSocketBase* __fastcall Clone();
	virtual void __fastcall StartSSL();
	virtual void __fastcall Close();
	virtual bool __fastcall Connected();
	virtual bool __fastcall Readable(int AMSec = 0xffffffff);
	System::UnicodeString __fastcall GetALPNProtocol();
	Sgcssl_schannel::TsgcSChannelConnectionInfo* __fastcall GetInfo();
	__property TCredentialsEvent OnCredentials = {read=FOnCredentials, write=SetCredentials};
	__property System::Classes::TStringList* ALPNProtocols = {read=GetALPNProtocols, write=SetALPNProtocols};
	__property Sgcssl_schannel::TsgcSChannelCertificate* Certificate = {read=FCertificate, write=SetCertificate};
	__property System::UnicodeString CipherList = {read=FCipherList, write=FCipherList};
	__property Sgcwebsocket_types::TwsTLSVersions TLSVersion = {read=FTLSVersion, write=FTLSVersion, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcssl_schannel_indy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCSSL_SCHANNEL_INDY)
using namespace Sgcssl_schannel_indy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcssl_schannel_indyHPP
