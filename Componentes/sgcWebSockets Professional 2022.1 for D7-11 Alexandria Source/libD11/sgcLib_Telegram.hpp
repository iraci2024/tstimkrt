// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcLib_Telegram.pas' rev: 35.00 (Windows)

#ifndef Sgclib_telegramHPP
#define Sgclib_telegramHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgclib_telegram
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef void * __cdecl (*Tsgctd_json_client_create)(void);

typedef void __cdecl (*Tsgctd_json_client_send)(void * Client, char * Request);

typedef char * __cdecl (*Tsgctd_json_client_receive)(void * Client, double Timeout);

typedef char * __cdecl (*Tsgctd_json_client_execute)(void * Client, char * Request);

typedef void __cdecl (*Tsgctd_json_client_destroy)(void * Client);

typedef void __fastcall (__closure *Tsgctd_fatal_error_event)(char * Error_Message);

typedef void __cdecl (*Tsgctd_set_log_fatal_error_callback)(Tsgctd_fatal_error_event OnFatalErrorCallback);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetTDJsonPath(const System::UnicodeString aPath);
extern DELPHI_PACKAGE void * __cdecl td_json_client_create(void);
extern DELPHI_PACKAGE void __cdecl td_json_client_send(void * Client, char * Request);
extern DELPHI_PACKAGE char * __cdecl td_json_client_receive(void * Client, double Timeout);
extern DELPHI_PACKAGE char * __cdecl td_json_client_execute(void * Client, char * Request);
extern DELPHI_PACKAGE void __cdecl td_json_client_destroy(void * Client);
extern DELPHI_PACKAGE void __cdecl td_set_log_fatal_error_callback(Tsgctd_fatal_error_event OnFatalErrorCallback);
}	/* namespace Sgclib_telegram */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCLIB_TELEGRAM)
using namespace Sgclib_telegram;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgclib_telegramHPP
