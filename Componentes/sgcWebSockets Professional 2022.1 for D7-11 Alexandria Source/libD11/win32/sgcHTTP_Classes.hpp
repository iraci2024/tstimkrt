// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_classesHPP
#define Sgchttp_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPComponent_Base;
class DELPHICLASS TsgcHTTPComponentClient_Base;
class DELPHICLASS TsgcHTTPComponentClientAuthToken_Base;
class DELPHICLASS TsgcHTTPComponentServer_Base;
class DELPHICLASS TsgcHTTPComponentServerAuthToken_Base;
class DELPHICLASS TsgcHTTPProxy_Options;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcOnAuthToken)(System::TObject* Sender, const System::UnicodeString TokenType, const System::UnicodeString Token, const System::UnicodeString Data);

typedef void __fastcall (__closure *TsgcOnAuthTokenError)(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString ErrorDescription, const System::UnicodeString Data);

class PASCALIMPLEMENTATION TsgcHTTPComponent_Base : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTPComponent_Base(System::Classes::TComponent* AOwner) : Sgcbase_classes::TsgcComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPComponent_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPComponentClient_Base : public TsgcHTTPComponent_Base
{
	typedef TsgcHTTPComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTPComponentClient_Base(System::Classes::TComponent* AOwner) : TsgcHTTPComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPComponentClient_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPComponentClientAuthToken_Base : public TsgcHTTPComponentClient_Base
{
	typedef TsgcHTTPComponentClient_Base inherited;
	
private:
	TsgcOnAuthTokenError FOnAuthTokenError;
	TsgcOnAuthToken FOnAuthToken;
	
protected:
	virtual void __fastcall DoAuthTokenEvent(const System::UnicodeString aTokenType, const System::UnicodeString aToken, const System::UnicodeString aData);
	virtual void __fastcall DoAuthTokenErrorEvent(const System::UnicodeString aError, const System::UnicodeString aErrorDescription, const System::UnicodeString aData);
	
public:
	__property TsgcOnAuthToken OnAuthToken = {read=FOnAuthToken, write=FOnAuthToken};
	__property TsgcOnAuthTokenError OnAuthTokenError = {read=FOnAuthTokenError, write=FOnAuthTokenError};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTPComponentClientAuthToken_Base(System::Classes::TComponent* AOwner) : TsgcHTTPComponentClient_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPComponentClientAuthToken_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPComponentServer_Base : public TsgcHTTPComponent_Base
{
	typedef TsgcHTTPComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTPComponentServer_Base(System::Classes::TComponent* AOwner) : TsgcHTTPComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPComponentServer_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPComponentServerAuthToken_Base : public TsgcHTTPComponentServer_Base
{
	typedef TsgcHTTPComponentServer_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcHTTPComponentServerAuthToken_Base(System::Classes::TComponent* AOwner) : TsgcHTTPComponentServer_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcHTTPComponentServerAuthToken_Base() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPProxy_Options : public Sgctcp_classes::TsgcTCPProxy_Options
{
	typedef Sgctcp_classes::TsgcTCPProxy_Options inherited;
	
public:
	/* TsgcSocketProxy_Options.Create */ inline __fastcall virtual TsgcHTTPProxy_Options() : Sgctcp_classes::TsgcTCPProxy_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPProxy_Options() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_CLASSES)
using namespace Sgchttp_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_classesHPP
