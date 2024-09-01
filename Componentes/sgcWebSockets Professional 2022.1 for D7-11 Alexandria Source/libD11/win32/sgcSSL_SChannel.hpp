// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcSSL_SChannel.pas' rev: 35.00 (Windows)

#ifndef Sgcssl_schannelHPP
#define Sgcssl_schannelHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.WinSock.hpp>
#include <System.Types.hpp>
#include <sgcSSL_WinSSPI.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcssl_schannel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSSLValidator;
class DELPHICLASS TsgcSChannelConnectionInfo;
class DELPHICLASS TsgcSChannelCertificate;
class DELPHICLASS TsgcSChannelStream;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSSLValidator : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall TSSLValidator();
	__fastcall virtual ~TSSLValidator();
	virtual bool __fastcall ValidateChain(Sgcssl_winsspi::PCCERT_CHAIN_CONTEXT Chain, Sgcssl_winsspi::CERT_CHAIN_POLICY_STATUS &Status);
	virtual bool __fastcall ValidateElement(Sgcssl_winsspi::PCERT_CHAIN_ELEMENT Element);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSChannelConnectionInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	unsigned FCiperStrength;
	System::UnicodeString FCipher;
	unsigned FCipherId;
	System::UnicodeString FExchange;
	unsigned FExchangeId;
	unsigned FExchangeStrength;
	System::UnicodeString FHash;
	unsigned FHashId;
	unsigned FHashStrength;
	Sgcwebsocket_types::TwsTLSVersions FProtocol;
	
protected:
	virtual void __fastcall SetProtocol(unsigned aValue);
	virtual void __fastcall SetCipher(unsigned aValue);
	virtual void __fastcall SetHash(unsigned aValue);
	virtual void __fastcall SetExchange(unsigned aValue);
	__property unsigned HashId = {read=FHashId, write=FHashId, nodefault};
	__property unsigned CipherId = {read=FCipherId, write=FCipherId, nodefault};
	__property unsigned ExchangeId = {read=FExchangeId, write=FExchangeId, nodefault};
	
public:
	__property unsigned CiperStrength = {read=FCiperStrength, write=FCiperStrength, nodefault};
	__property System::UnicodeString Cipher = {read=FCipher, write=FCipher};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property unsigned ExchangeStrength = {read=FExchangeStrength, write=FExchangeStrength, nodefault};
	__property System::UnicodeString Hash = {read=FHash, write=FHash};
	__property unsigned HashStrength = {read=FHashStrength, write=FHashStrength, nodefault};
	__property Sgcwebsocket_types::TwsTLSVersions Protocol = {read=FProtocol, write=FProtocol, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcSChannelConnectionInfo() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSChannelConnectionInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSChannelCertificate : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FCertFilePassword;
	System::UnicodeString FCertHash;
	System::UnicodeString FCertFile;
	Sgcwebsocket_types::TsgcSChannelCertStoreName FCertStoreName;
	Sgcwebsocket_types::TsgcSChannelCertStorePath FCertStorePath;
	bool FVerifyCertificate;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString CertFilePassword = {read=FCertFilePassword, write=FCertFilePassword};
	__property System::UnicodeString CertHash = {read=FCertHash, write=FCertHash};
	__property System::UnicodeString CertFile = {read=FCertFile, write=FCertFile};
	__property Sgcwebsocket_types::TsgcSChannelCertStoreName CertStoreName = {read=FCertStoreName, write=FCertStoreName, nodefault};
	__property Sgcwebsocket_types::TsgcSChannelCertStorePath CertStorePath = {read=FCertStorePath, write=FCertStorePath, nodefault};
	__property bool VerifyCertificate = {read=FVerifyCertificate, write=FVerifyCertificate, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSChannelCertificate() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcSChannelCertificate() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSChannelStream : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Byte *FData;
	int FDataSize;
	
protected:
	void __fastcall DoFinalize();
	void __fastcall DoInitialize(int aSize);
	
public:
	__fastcall virtual ~TsgcSChannelStream();
	void __fastcall LoadFromStream(System::Classes::TStream* const aSource);
	void __fastcall LoadFromFile(const System::UnicodeString aFileName);
	__property System::PByte Data = {read=FData};
	__property int DataSize = {read=FDataSize, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcSChannelStream() : System::TObject() { }
	
};

#pragma pack(pop)

typedef void __fastcall (*TCredentialsCallBack)(NativeUInt SSL, void * UserData);

typedef System::StaticArray<System::UnicodeString, 50> Sgcssl_schannel__5;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE NativeUInt secur32;
extern DELPHI_PACKAGE unsigned CertStatus;
extern DELPHI_PACKAGE System::UnicodeString vMsgError;
extern DELPHI_PACKAGE HRESULT oError;
extern DELPHI_PACKAGE Sgcssl_schannel__5 CS_SChannel_Ciphers;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 50> CS_SChannel_CiphersId;
static const System::Word CS_SChannel_ERROR_INTERRUPTED_SYSTEM_CALL = System::Word(0x2714);
extern DELPHI_PACKAGE bool __fastcall SSLAvailable(void);
extern DELPHI_PACKAGE int __fastcall SSLStart(NativeInt aSocket, TsgcSChannelCertificate* aCertificate, const System::AnsiString Host = System::AnsiString(), Sgcwebsocket_types::TwsTLSVersions aVersion = (Sgcwebsocket_types::TwsTLSVersions)(0x1), System::Classes::TStrings* aALPNProtocols = (System::Classes::TStrings*)(0x0), System::UnicodeString aCipherList = System::UnicodeString());
extern DELPHI_PACKAGE void __fastcall SSLCredentialsCallBack(NativeUInt SSL, TCredentialsCallBack CallBack, void * UserData);
extern DELPHI_PACKAGE bool __fastcall SSLPending(NativeUInt SSL);
extern DELPHI_PACKAGE int __fastcall SSLRead(NativeUInt SSL, void *Data, int Size);
extern DELPHI_PACKAGE int __fastcall SSLWrite(NativeUInt SSL, void *Data, int Size);
extern DELPHI_PACKAGE int __fastcall SSLClose(NativeUInt SSL);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetALPN(NativeUInt SSL);
extern DELPHI_PACKAGE TsgcSChannelConnectionInfo* __fastcall GetConnectionInfo(NativeUInt SSL);
}	/* namespace Sgcssl_schannel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCSSL_SCHANNEL)
using namespace Sgcssl_schannel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcssl_schannelHPP
