// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Classes_Queues.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_classes_queuesHPP
#define Sgcwebsocket_classes_queuesHPP

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
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_classes_queues
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcQueueItemBase;
class DELPHICLASS TsgcQueueNameItemBase;
class DELPHICLASS TsgcQueueCommon;
class DELPHICLASS TsgcQueueBase;
class DELPHICLASS TsgcFIFOQueue;
class DELPHICLASS TsgcQueue;
class DELPHICLASS TsgcCacheQueue;
class DELPHICLASS TsgcCacheQueueListBase;
class DELPHICLASS TsgcQueueNameBase;
class DELPHICLASS TsgcQueueNameListBase;
class DELPHICLASS TsgcCacheQueueList;
class DELPHICLASS TsgcCacheQueueFullList;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcQueueItemBase : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	__int64 FOrder;
	virtual __int64 __fastcall GetOrder();
	System::UnicodeString FID;
	virtual System::UnicodeString __fastcall GetID();
	
public:
	__fastcall virtual TsgcQueueItemBase();
	__property System::UnicodeString ID = {read=GetID, write=FID};
	__property __int64 Order = {read=GetOrder, write=FOrder};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcQueueItemBase() { }
	
};


class PASCALIMPLEMENTATION TsgcQueueNameItemBase : public TsgcQueueItemBase
{
	typedef TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FQueue;
	
public:
	__fastcall virtual TsgcQueueNameItemBase();
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcQueueNameItemBase() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueCommon : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
private:
	bool FOwnObjects;
	int __fastcall GetCount();
	
public:
	__fastcall virtual TsgcQueueCommon();
	__fastcall virtual ~TsgcQueueCommon();
	virtual void __fastcall Clear();
	__property int Count = {read=GetCount, nodefault};
	__property bool OwnObjects = {read=FOwnObjects, write=FOwnObjects, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueBase : public TsgcQueueCommon
{
	typedef TsgcQueueCommon inherited;
	
public:
	TsgcQueueItemBase* operator[](int Index) { return this->Item[Index]; }
	
protected:
	virtual TsgcQueueItemBase* __fastcall GetItemByIndex(int Index);
	
public:
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	bool __fastcall DeleteItem(const System::UnicodeString aID)/* overload */;
	bool __fastcall DeleteItem(const int Index)/* overload */;
	TsgcQueueItemBase* __fastcall GetItem(const System::UnicodeString aID)/* overload */;
	TsgcQueueItemBase* __fastcall GetItem(const int Index)/* overload */;
	__property TsgcQueueItemBase* Item[int Index] = {read=GetItemByIndex/*, default*/};
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcQueueBase() : TsgcQueueCommon() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcQueueBase() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcFIFOQueue : public TsgcQueueBase
{
	typedef TsgcQueueBase inherited;
	
private:
	int FMaxItems;
	
public:
	__fastcall virtual TsgcFIFOQueue();
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	__property int MaxItems = {read=FMaxItems, write=FMaxItems, nodefault};
public:
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcFIFOQueue() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueue : public TsgcQueueBase
{
	typedef TsgcQueueBase inherited;
	
public:
	void __fastcall SortByOrder(bool aAscending = true);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcQueue() : TsgcQueueBase() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcQueue() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcCacheQueue : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	TsgcQueueItemBase* operator[](int Index) { return this->Item[Index]; }
	
private:
	int __fastcall GetCount();
	
protected:
	virtual TsgcQueueItemBase* __fastcall GetItemByIndex(int Index);
	
private:
	TsgcFIFOQueue* FCache;
	TsgcQueue* FQueue;
	System::UnicodeString FQueueName;
	TsgcFIFOQueue* __fastcall GetCache();
	int __fastcall GetCacheSize();
	bool __fastcall GetOwnObjects();
	TsgcQueue* __fastcall GetQueue();
	void __fastcall SetCacheSize(const int Value);
	void __fastcall SetOwnObjects(const bool Value);
	
protected:
	__property TsgcFIFOQueue* Cache = {read=GetCache, write=FCache};
	__property TsgcQueue* Queue = {read=GetQueue, write=FQueue};
	
public:
	void __fastcall Clear();
	__fastcall virtual TsgcCacheQueue(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcCacheQueue();
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	bool __fastcall DeleteItem(const System::UnicodeString aID);
	TsgcQueueItemBase* __fastcall GetItem(const System::UnicodeString aID);
	__property TsgcQueueItemBase* Item[int Index] = {read=GetItemByIndex/*, default*/};
	__property int CacheSize = {read=GetCacheSize, write=SetCacheSize, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property bool OwnObjects = {read=GetOwnObjects, write=SetOwnObjects, nodefault};
	__property System::UnicodeString QueueName = {read=FQueueName, write=FQueueName};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcCacheQueueListBase : public TsgcQueueCommon
{
	typedef TsgcQueueCommon inherited;
	
private:
	int FCacheSize;
	int FGroupLevel;
	HIDESBASE int __fastcall GetCount();
	System::UnicodeString __fastcall GetQueueName(const System::UnicodeString aID);
	
protected:
	TsgcCacheQueue* __fastcall AddQueue(const System::UnicodeString aQueueName);
	TsgcCacheQueue* __fastcall GetQueue(const System::UnicodeString aQueueName);
	bool __fastcall DeleteQueue(const System::UnicodeString aQueueName);
	
public:
	__fastcall virtual TsgcCacheQueueListBase();
	__fastcall virtual ~TsgcCacheQueueListBase();
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	bool __fastcall DeleteItem(const System::UnicodeString aID);
	TsgcQueueItemBase* __fastcall GetItem(const System::UnicodeString aID);
	__property int CacheSize = {read=FCacheSize, write=FCacheSize, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property int GroupLevel = {read=FGroupLevel, write=FGroupLevel, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueNameBase : public TsgcQueueBase
{
	typedef TsgcQueueBase inherited;
	
private:
	System::UnicodeString FQueueName;
	
public:
	__property System::UnicodeString QueueName = {read=FQueueName, write=FQueueName};
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcQueueNameBase() : TsgcQueueBase() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcQueueNameBase() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcQueueNameListBase : public TsgcQueueCommon
{
	typedef TsgcQueueCommon inherited;
	
private:
	HIDESBASE int __fastcall GetCount();
	
protected:
	TsgcQueueNameBase* __fastcall AddQueue(const System::UnicodeString aQueueName);
	TsgcQueueNameBase* __fastcall GetQueue(const System::UnicodeString aQueueName);
	bool __fastcall DeleteQueue(const System::UnicodeString aQueueName);
	
public:
	__fastcall virtual TsgcQueueNameListBase();
	__fastcall virtual ~TsgcQueueNameListBase();
	virtual bool __fastcall AddItem(TsgcQueueNameItemBase* const aItem);
	void __fastcall DeleteItem(const System::UnicodeString aID)/* overload */;
	bool __fastcall DeleteItem(const System::UnicodeString aQueueName, const System::UnicodeString aID)/* overload */;
	TsgcQueueNameBase* __fastcall GetQueueByName(const System::UnicodeString aQueueName);
	System::UnicodeString __fastcall GetQueuesDelimitedText();
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcCacheQueueList : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TsgcCacheQueueListBase* FQueueCache;
	int __fastcall GetCacheSize();
	int __fastcall GetGroupLevel();
	void __fastcall SetCacheSize(const int Value);
	void __fastcall SetGroupLevel(const int Value);
	
protected:
	virtual int __fastcall GetCount();
	__property TsgcCacheQueueListBase* QueueCache = {read=FQueueCache, write=FQueueCache};
	
public:
	__fastcall virtual TsgcCacheQueueList(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcCacheQueueList();
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	virtual bool __fastcall DeleteItem(const System::UnicodeString aID);
	TsgcQueueItemBase* __fastcall GetItem(const System::UnicodeString aID);
	__property int CacheSize = {read=GetCacheSize, write=SetCacheSize, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property int GroupLevel = {read=GetGroupLevel, write=SetGroupLevel, nodefault};
};


class PASCALIMPLEMENTATION TsgcCacheQueueFullList : public TsgcCacheQueueList
{
	typedef TsgcCacheQueueList inherited;
	
public:
	TsgcQueueItemBase* operator[](int Index) { return this->Item[Index]; }
	
private:
	TsgcQueueBase* FQueue;
	
protected:
	virtual int __fastcall GetCount();
	virtual TsgcQueueItemBase* __fastcall GetItemByIndex(int Index);
	__property TsgcQueueBase* Queue = {read=FQueue, write=FQueue};
	
public:
	__fastcall virtual TsgcCacheQueueFullList(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcCacheQueueFullList();
	virtual void __fastcall AddItem(TsgcQueueItemBase* const aItem);
	virtual bool __fastcall DeleteItem(const System::UnicodeString aID);
	__property TsgcQueueItemBase* Item[int Index] = {read=GetItemByIndex/*, default*/};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_classes_queues */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLASSES_QUEUES)
using namespace Sgcwebsocket_classes_queues;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_classes_queuesHPP
