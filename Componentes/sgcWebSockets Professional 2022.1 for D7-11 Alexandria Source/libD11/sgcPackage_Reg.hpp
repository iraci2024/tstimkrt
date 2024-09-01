// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcPackage_Reg.pas' rev: 35.00 (Windows)

#ifndef Sgcpackage_regHPP
#define Sgcpackage_regHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ToolsAPI.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcpackage_reg
{
//-- forward type declarations -----------------------------------------------
struct TsgcInstanceEvent;
struct TsgcInstanceGlobalInfo;
struct TsgcInstanceLocalInfo;
class DELPHICLASS TsgcInstanceCounter;
class DELPHICLASS TsgcInstanceCounterThread;
class DELPHICLASS TsgcCheckPackage;
class DELPHICLASS TIDEIOTAsgcWebSockets;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcInstanceEventType : unsigned char { ieGlobalCreated, ieGlobalDeleted, ieLocalRequest, ieLocalStatus, ieGlobalRequest, ieGlobalStatus };

struct DECLSPEC_DRECORD TsgcInstanceEvent
{
public:
	unsigned Signature;
	unsigned Id;
	TsgcInstanceEventType Event;
	System::SmallString<16> Computer;
	int LocalIndex;
	int InstanceCount;
	unsigned PID;
};


typedef TsgcInstanceEvent *PsgcInstanceEvent;

struct DECLSPEC_DRECORD TsgcInstanceGlobalInfo
{
public:
	System::SmallString<16> Computer;
	int LocalCount;
	System::TDateTime LastRefresh;
};


typedef TsgcInstanceGlobalInfo *PsgcInstanceGlobalInfo;

struct DECLSPEC_DRECORD TsgcInstanceLocalInfo
{
public:
	unsigned PID;
	System::TDateTime LastRefresh;
};


typedef TsgcInstanceLocalInfo *PsgcInstanceLocalInfo;

class PASCALIMPLEMENTATION TsgcInstanceCounter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool _Active;
	int _UpdateCount;
	int _Interval;
	System::UnicodeString _InstanceName;
	System::Classes::TNotifyEvent _OnChange;
	void __fastcall RemoveExpiredInfo();
	void __fastcall SetInterval(const int Value);
	void __fastcall SetInstanceName(const System::UnicodeString Value);
	System::Classes::TStringList* _ReceivedEvents;
	System::Classes::TThreadList* _ThreadLocalInfo;
	System::Classes::TThreadList* _ThreadGlobalInfo;
	System::Classes::TList* _LocalInfo;
	System::Classes::TList* _GlobalInfo;
	int _LockCount;
	void __fastcall Lock();
	void __fastcall Unlock();
	NativeUInt _ThreadStop;
	NativeUInt _ThreadStopped;
	TsgcInstanceCounterThread* _Thread;
	
protected:
	void __fastcall SetActive(bool Value);
	void __fastcall ApplyUpdates();
	
public:
	int __fastcall CountComputers();
	int __fastcall CountInstances();
	void __fastcall RequestUpdate();
	__fastcall TsgcInstanceCounter();
	__fastcall virtual ~TsgcInstanceCounter();
	__property bool Active = {read=_Active, write=SetActive, nodefault};
	void __fastcall BeginUpdates();
	void __fastcall EndUpdates();
	__property System::UnicodeString InstanceName = {read=_InstanceName, write=SetInstanceName};
	__property int Interval = {read=_Interval, write=SetInterval, default=10000};
	__property System::Classes::TNotifyEvent OnChange = {read=_OnChange, write=_OnChange};
};


class PASCALIMPLEMENTATION TsgcInstanceCounterThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcInstanceCounter* _Owner;
	System::Classes::TList* _LocalInfo;
	System::Classes::TList* _GlobalInfo;
	System::TDateTime _IntervalTime;
	int _LocalIndex;
	NativeUInt _Slot;
	System::UnicodeString _SlotName;
	bool _Changed;
	int _LockCount;
	void __fastcall Changed();
	void __fastcall Lock();
	void __fastcall Unlock();
	void __fastcall InitSlot();
	void __fastcall DoneSlot();
	void __fastcall UpdateSlot();
	bool __fastcall GetInstanceEvent(TsgcInstanceEvent &Event);
	void __fastcall ProcessInstanceEvent(const TsgcInstanceEvent &Event);
	PsgcInstanceLocalInfo __fastcall FindLocal(const TsgcInstanceEvent &Event);
	void __fastcall PostLocal(const TsgcInstanceEvent &Event, int LocalIndex);
	void __fastcall PostLocalStatus(int LocalIndex);
	void __fastcall PostLocalRequest();
	PsgcInstanceGlobalInfo __fastcall FindGlobal(const TsgcInstanceEvent &Event);
	void __fastcall PostGlobal(const TsgcInstanceEvent &Event, System::UnicodeString Computer, int LocalIndex);
	void __fastcall PostGlobalStatus(System::UnicodeString Computer, int LocalIndex);
	void __fastcall PostGlobalRequest();
	void __fastcall InitEvent(TsgcInstanceEvent &Event);
	void __fastcall DoPostEvent(const TsgcInstanceEvent &Event, System::UnicodeString SlotName);
	void __fastcall ProcessLocalCreated(const TsgcInstanceEvent &Event);
	void __fastcall ProcessLocalDeleted(const TsgcInstanceEvent &Event);
	void __fastcall ProcessGlobalCreated(const TsgcInstanceEvent &Event);
	void __fastcall ProcessGlobalDeleted(const TsgcInstanceEvent &Event);
	void __fastcall ProcessLocalRequest(const TsgcInstanceEvent &Event);
	void __fastcall ProcessLocalStatus(const TsgcInstanceEvent &Event);
	void __fastcall ProcessGlobalRequest(const TsgcInstanceEvent &Event);
	void __fastcall ProcessGlobalStatus(const TsgcInstanceEvent &Event);
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TsgcInstanceCounterThread(TsgcInstanceCounter* Owner);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TsgcInstanceCounterThread() { }
	
};


typedef void __fastcall (*TFuncCallback)(void);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcCheckPackage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcInstanceCounter* FInstanceCounter;
	bool FIsValid;
	int FInstanceCount;
	TFuncCallback FCallBack;
	void __fastcall Change(System::TObject* Sender);
	void __fastcall SetCallBack(const TFuncCallback Value);
	
public:
	__property bool IsValid = {read=FIsValid, nodefault};
	__property int InstanceCount = {read=FInstanceCount, nodefault};
	__fastcall TsgcCheckPackage(System::UnicodeString aPackage);
	__fastcall virtual ~TsgcCheckPackage();
	__property TFuncCallback CallBack = {read=FCallBack, write=SetCallBack};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TIDEIOTAsgcWebSockets : public Toolsapi::TNotifierObject
{
	typedef Toolsapi::TNotifierObject inherited;
	
private:
	TsgcCheckPackage* FCheckPackage;
	
public:
	void __fastcall FileNotification(Toolsapi::TOTAFileNotification NotifyCode, const System::UnicodeString FileName, bool &Cancel);
	void __fastcall BeforeCompile(const Toolsapi::_di_IOTAProject Project, bool &Cancel)/* overload */;
	void __fastcall AfterCompile(bool Succeeded)/* overload */;
	void __fastcall BeforeCompile(const Toolsapi::_di_IOTAProject Project, bool IsCodeInsight, bool &Cancel)/* overload */;
	void __fastcall AfterCompile(bool Succeeded, bool IsCodeInsight)/* overload */;
	__property TsgcCheckPackage* CheckPackage = {read=FCheckPackage, write=FCheckPackage};
public:
	/* TObject.Create */ inline __fastcall TIDEIOTAsgcWebSockets() : Toolsapi::TNotifierObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TIDEIOTAsgcWebSockets() { }
	
private:
	void *__IOTAIDENotifier50;	// Toolsapi::IOTAIDENotifier50 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {AC7D29F1-D9A9-11D2-A8C1-00C04FA32F53}
	operator Toolsapi::_di_IOTAIDENotifier50()
	{
		Toolsapi::_di_IOTAIDENotifier50 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAIDENotifier50*(void) { return (Toolsapi::IOTAIDENotifier50*)&__IOTAIDENotifier50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E052204F-ECE9-11D1-AB19-00C04FB16FB3}
	operator Toolsapi::_di_IOTAIDENotifier()
	{
		Toolsapi::_di_IOTAIDENotifier intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAIDENotifier*(void) { return (Toolsapi::IOTAIDENotifier*)&__IOTAIDENotifier50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {F17A7BCF-E07D-11D1-AB0B-00C04FB16FB3}
	operator Toolsapi::_di_IOTANotifier()
	{
		Toolsapi::_di_IOTANotifier intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTANotifier*(void) { return (Toolsapi::IOTANotifier*)&__IOTAIDENotifier50; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CS_COMPUTER_LENGTH = System::Int8(0xf);
static const unsigned CS_EVENT_SIGNATURE = unsigned(0xeab6f31c);
}	/* namespace Sgcpackage_reg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCPACKAGE_REG)
using namespace Sgcpackage_reg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcpackage_regHPP
