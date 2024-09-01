// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Classes_SyncObjs.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_classes_syncobjsHPP
#define Sgcwebsocket_classes_syncobjsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <System.SyncObjs.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_classes_syncobjs
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcCriticalSection;
class DELPHICLASS TsgcThreadList;
class DELPHICLASS TsgcThreadSafeInteger;
class DELPHICLASS TsgcThreadSafeStringList;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcThreadListType : unsigned char { tdlThreadDictionary, tdlOnlyThread, tdlOnlyDictionary };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcCriticalSection : public System::Syncobjs::TSynchroObject
{
	typedef System::Syncobjs::TSynchroObject inherited;
	
private:
	_RTL_CRITICAL_SECTION FSection;
	
public:
	__fastcall TsgcCriticalSection();
	__fastcall virtual ~TsgcCriticalSection();
	virtual void __fastcall Acquire();
	virtual void __fastcall Release();
	bool __fastcall TryAcquire();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FList;
	TsgcCriticalSection* FLock;
	System::Types::TDuplicates FDuplicates;
	TsgcThreadListType FListType;
	bool __fastcall GetIsEmpty();
	TsgcThreadListType __fastcall GetListType();
	void __fastcall SetListType(const TsgcThreadListType Value);
	
public:
	__fastcall virtual TsgcThreadList();
	__fastcall virtual ~TsgcThreadList();
	void __fastcall Add(void * Item);
	void __fastcall AddKey(void * Item, const System::UnicodeString aKey);
	void __fastcall Delete(int Index);
	void __fastcall DeleteKey(int Index, const System::UnicodeString aKey);
	System::TObject* __fastcall GetValue(const System::UnicodeString aKey);
	virtual void __fastcall Clear();
	System::Classes::TList* __fastcall LockList();
	void __fastcall Remove(void * Item);
	void __fastcall RemoveItem(void * Item, System::Types::TDirection Direction);
	void __fastcall UnlockList();
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool IsEmpty = {read=GetIsEmpty, nodefault};
	__property TsgcThreadListType ListType = {read=GetListType, write=SetListType, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcThreadSafeInteger : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcCriticalSection* FLock;
	__int64 FValue;
	
public:
	__fastcall virtual TsgcThreadSafeInteger();
	__fastcall virtual ~TsgcThreadSafeInteger();
	__int64 __fastcall Inc();
	__int64 __fastcall Dec();
	__property __int64 Value = {read=FValue};
};


class PASCALIMPLEMENTATION TsgcThreadSafeStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TsgcCriticalSection* FLock;
	
public:
	__fastcall TsgcThreadSafeStringList()/* overload */;
	__fastcall virtual ~TsgcThreadSafeStringList();
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
public:
	/* TStringList.Create */ inline __fastcall TsgcThreadSafeStringList(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Create */ inline __fastcall TsgcThreadSafeStringList(System::WideChar QuoteChar, System::WideChar Delimiter)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter) { }
	/* TStringList.Create */ inline __fastcall TsgcThreadSafeStringList(System::WideChar QuoteChar, System::WideChar Delimiter, System::Classes::TStringsOptions Options)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter, Options) { }
	/* TStringList.Create */ inline __fastcall TsgcThreadSafeStringList(System::Types::TDuplicates Duplicates, bool Sorted, bool CaseSensitive)/* overload */ : System::Classes::TStringList(Duplicates, Sorted, CaseSensitive) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_classes_syncobjs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLASSES_SYNCOBJS)
using namespace Sgcwebsocket_classes_syncobjs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_classes_syncobjsHPP
