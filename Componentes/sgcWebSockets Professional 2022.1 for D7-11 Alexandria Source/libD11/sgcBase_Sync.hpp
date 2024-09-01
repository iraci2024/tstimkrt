// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcBase_Sync.pas' rev: 35.00 (Windows)

#ifndef Sgcbase_syncHPP
#define Sgcbase_syncHPP

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

//-- user supplied -----------------------------------------------------------

namespace Sgcbase_sync
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSyncMessages;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcSyncOnMessage)(System::TObject* Sender, const System::UnicodeString Value);

class PASCALIMPLEMENTATION TsgcSyncMessages : public Sgcbase_classes::TsgcThreadSafeBase
{
	typedef Sgcbase_classes::TsgcThreadSafeBase inherited;
	
private:
	System::Classes::TStringList* FData;
	TsgcSyncOnMessage FOnMessage;
	System::Classes::TStringList* __fastcall GetData();
	
protected:
	__property System::Classes::TStringList* Data = {read=GetData, write=FData};
	void __fastcall DoData();
	
public:
	void __fastcall Synchronize(const System::UnicodeString aText);
	void __fastcall Queue(const System::UnicodeString aText);
	__property TsgcSyncOnMessage OnMessage = {read=FOnMessage, write=FOnMessage};
public:
	/* TsgcThreadSafeBase.Create */ inline __fastcall virtual TsgcSyncMessages() : Sgcbase_classes::TsgcThreadSafeBase() { }
	/* TsgcThreadSafeBase.Destroy */ inline __fastcall virtual ~TsgcSyncMessages() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TsgcSyncMessages* __fastcall sgcSyncMessages(void);
}	/* namespace Sgcbase_sync */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCBASE_SYNC)
using namespace Sgcbase_sync;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcbase_syncHPP
