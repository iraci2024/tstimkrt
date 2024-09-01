// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcLibs.pas' rev: 35.00 (Windows)

#ifndef SgclibsHPP
#define SgclibsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcLib_RCON_Client.hpp>
#include <sgcHTTP_API_Cryptohopper.hpp>
#include <sgcLib_Telegram_Client.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcHTTP_API.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgclibs
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcTDLib_Telegram;
class DELPHICLASS TsgcLib_RCON;
class DELPHICLASS TsgcHTTP_Cryptohopper;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcTDLib_Telegram : public Sgclib_telegram_client::TsgcTDLib_Telegram_Client
{
	typedef Sgclib_telegram_client::TsgcTDLib_Telegram_Client inherited;
	
__published:
	__property OnBeforeReadEvent;
	__property OnConnectionStatus;
	__property OnAuthorizationStatus;
	__property OnAuthenticationCode;
	__property OnAuthenticationPassword;
	__property OnRegisterUser;
	__property OnMessageText;
	__property OnMessageDocument;
	__property OnMessagePhoto;
	__property OnMessageVideo;
	__property OnNewChat;
	__property OnNewCallbackQuery;
	__property OnEvent;
	__property OnException;
	__property Telegram;
	__property Version = {default=0};
public:
	/* TsgcTDLib_Telegram_Client.Create */ inline __fastcall virtual TsgcTDLib_Telegram(System::Classes::TComponent* aOwner) : Sgclib_telegram_client::TsgcTDLib_Telegram_Client(aOwner) { }
	/* TsgcTDLib_Telegram_Client.Destroy */ inline __fastcall virtual ~TsgcTDLib_Telegram() { }
	
};


class PASCALIMPLEMENTATION TsgcLib_RCON : public Sgclib_rcon_client::TsgcLib_RCON_Client
{
	typedef Sgclib_rcon_client::TsgcLib_RCON_Client inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnAuthenticate;
	__property OnResponse;
	__property OnException;
	__property RCON_Options;
	__property NotifyEvents;
	__property Version = {default=0};
public:
	/* TsgcLib_RCON_Client.Create */ inline __fastcall virtual TsgcLib_RCON(System::Classes::TComponent* aOwner) : Sgclib_rcon_client::TsgcLib_RCON_Client(aOwner) { }
	/* TsgcLib_RCON_Client.Destroy */ inline __fastcall virtual ~TsgcLib_RCON() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_Cryptohopper : public Sgchttp_api_cryptohopper::TsgcHTTP_API_Cryptohopper
{
	typedef Sgchttp_api_cryptohopper::TsgcHTTP_API_Cryptohopper inherited;
	
__published:
	__property OnWebhook;
	__property CryptohopperOptions;
	__property Version = {default=0};
public:
	/* TsgcHTTP_API_Cryptohopper.Create */ inline __fastcall virtual TsgcHTTP_Cryptohopper(System::Classes::TComponent* aOwner) : Sgchttp_api_cryptohopper::TsgcHTTP_API_Cryptohopper(aOwner) { }
	/* TsgcHTTP_API_Cryptohopper.Destroy */ inline __fastcall virtual ~TsgcHTTP_Cryptohopper() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgclibs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCLIBS)
using namespace Sgclibs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgclibsHPP
