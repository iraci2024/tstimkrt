// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcBase_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcbase_classesHPP
#define Sgcbase_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <Winapi.Windows.hpp>
#include <IdGlobal.hpp>
#include <IdThread.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcbase_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcComponent_Base;
class DELPHICLASS TsgcIdThread;
class DELPHICLASS TsgcTimer;
class DELPHICLASS TsgcThreadSafeBase;
class DELPHICLASS TsgcThreadSafeComponent;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcComponent_Base : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString __fastcall GetVersion();
	
protected:
	virtual bool __fastcall IsDesigning();
	virtual bool __fastcall IsDestroying();
	virtual bool __fastcall IsLoading();
	
public:
	__property System::UnicodeString Version = {read=GetVersion};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcComponent_Base(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcComponent_Base() { }
	
};


typedef void __fastcall (__closure *TsgcTimerOnException)(System::TObject* Sender, System::Sysutils::Exception* E);

class PASCALIMPLEMENTATION TsgcIdThread : public Idthread::TIdThread
{
	typedef Idthread::TIdThread inherited;
	
private:
	System::UnicodeString FDebugName;
	unsigned FThreadId;
	
protected:
	bool FIsExecuting;
	virtual void __fastcall Run();
	
public:
	__fastcall virtual TsgcIdThread(bool ACreateSuspended, bool ALoop, const System::UnicodeString AName);
	bool __fastcall IsCurrentThread();
	__property System::UnicodeString DebugName = {read=FDebugName, write=FDebugName};
	__property bool IsExecuting = {read=FIsExecuting, nodefault};
public:
	/* TIdThread.Destroy */ inline __fastcall virtual ~TsgcIdThread() { }
	
};


class PASCALIMPLEMENTATION TsgcTimer : public TsgcIdThread
{
	typedef TsgcIdThread inherited;
	
private:
	bool FEnabled;
	int FInterval;
	Sgcwebsocket_types::TwsNotifyEvent FNotifyEvents;
	TsgcTimerOnException FOnException;
	System::Classes::TNotifyEvent FOnTimer;
	void __fastcall SetEnabled(const bool Value);
	
protected:
	virtual void __fastcall DoOnTimerEvent();
	virtual void __fastcall DoOnExceptionEvent(System::Sysutils::Exception* const E);
	virtual void __fastcall Run();
	
public:
	__fastcall TsgcTimer()/* overload */;
	__fastcall virtual ~TsgcTimer();
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=FNotifyEvents, write=FNotifyEvents, nodefault};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property TsgcTimerOnException OnException = {read=FOnException, write=FOnException};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadSafeBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	_RTL_CRITICAL_SECTION FCS;
	
protected:
	void __fastcall DoEnterCS();
	void __fastcall DoLeaveCS();
	
public:
	__fastcall virtual TsgcThreadSafeBase();
	__fastcall virtual ~TsgcThreadSafeBase();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcThreadSafeComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	_RTL_CRITICAL_SECTION FCS;
	
protected:
	void __fastcall DoEnterCS();
	void __fastcall DoLeaveCS();
	
public:
	__fastcall virtual TsgcThreadSafeComponent(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcThreadSafeComponent();
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall sgcThreadFree(TsgcIdThread* const &aThread);
}	/* namespace Sgcbase_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCBASE_CLASSES)
using namespace Sgcbase_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcbase_classesHPP
