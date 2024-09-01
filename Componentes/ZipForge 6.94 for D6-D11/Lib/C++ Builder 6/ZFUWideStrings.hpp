// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFUWideStrings.pas' rev: 6.00

#ifndef ZFUWideStringsHPP
#define ZFUWideStringsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfuwidestrings
{
//-- type declarations -------------------------------------------------------
__interface IWideStringsAdapter;
typedef System::DelphiInterface<IWideStringsAdapter> _di_IWideStringsAdapter;
class DELPHICLASS TWideStrings;
__interface INTERFACE_UUID("{25FE0E3B-66CB-48AA-B23B-BCFA67E8F5DA}") IWideStringsAdapter  : public IInterface 
{
	
public:
	virtual void __fastcall ReferenceStrings(TWideStrings* S) = 0 ;
	virtual void __fastcall ReleaseStrings(void) = 0 ;
};

class DELPHICLASS TWideStringsEnumerator;
class PASCALIMPLEMENTATION TWideStringsEnumerator : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FIndex;
	TWideStrings* FStrings;
	
public:
	__fastcall TWideStringsEnumerator(TWideStrings* AStrings);
	WideString __fastcall GetCurrent();
	bool __fastcall MoveNext(void);
	__property WideString Current = {read=GetCurrent};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TWideStringsEnumerator(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TWideStrings : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
public:
	WideString operator[](int Index) { return Strings[Index]; }
	
private:
	Classes::TStringsDefined FDefined;
	wchar_t FDelimiter;
	wchar_t FQuoteChar;
	int FUpdateCount;
	_di_IWideStringsAdapter FAdapter;
	WideString __fastcall GetCommaText();
	WideString __fastcall GetDelimitedText();
	WideString __fastcall GetName(int Index);
	WideString __fastcall GetValue(const WideString Name);
	void __fastcall ReadData(Classes::TReader* Reader);
	void __fastcall SetCommaText(const WideString Value);
	void __fastcall SetDelimitedText(const WideString Value);
	void __fastcall SetStringsAdapter(const _di_IWideStringsAdapter Value);
	void __fastcall SetValue(const WideString Name, const WideString Value);
	void __fastcall WriteData(Classes::TWriter* Writer);
	wchar_t __fastcall GetDelimiter(void);
	void __fastcall SetDelimiter(const wchar_t Value);
	wchar_t __fastcall GetQuoteChar(void);
	void __fastcall SetQuoteChar(const wchar_t Value);
	wchar_t __fastcall GetNameValueSeparator(void);
	WideString __fastcall GetValueFromIndex(int Index);
	void __fastcall SetValueFromIndex(int Index, const WideString Value);
	
protected:
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	void __fastcall Error(const WideString Msg, int Data)/* overload */;
	void __fastcall Error(System::PResStringRec Msg, int Data)/* overload */;
	WideString __fastcall ExtractName(const WideString S);
	virtual WideString __fastcall Get(int Index) = 0 ;
	virtual int __fastcall GetCapacity(void);
	virtual int __fastcall GetCount(void) = 0 ;
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual WideString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetTextStr(const WideString Value);
	virtual void __fastcall SetUpdateState(bool Updating);
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	virtual int __fastcall CompareStrings(const WideString S1, const WideString S2);
	
public:
	__fastcall virtual ~TWideStrings(void);
	virtual int __fastcall Add(const WideString S);
	virtual int __fastcall AddObject(const WideString S, System::TObject* AObject);
	void __fastcall Append(const WideString S);
	virtual void __fastcall AddStrings(Classes::TStrings* Strings)/* overload */;
	virtual void __fastcall AddStrings(TWideStrings* Strings)/* overload */;
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	void __fastcall BeginUpdate(void);
	virtual void __fastcall Clear(void) = 0 ;
	virtual void __fastcall Delete(int Index) = 0 ;
	void __fastcall EndUpdate(void);
	bool __fastcall Equals(TWideStrings* Strings);
	virtual void __fastcall Exchange(int Index1, int Index2);
	TWideStringsEnumerator* __fastcall GetEnumerator(void);
	virtual wchar_t * __fastcall GetTextW(void);
	virtual int __fastcall IndexOf(const WideString S);
	virtual int __fastcall IndexOfName(const WideString Name);
	virtual int __fastcall IndexOfObject(System::TObject* AObject);
	virtual void __fastcall Insert(int Index, const WideString S) = 0 ;
	virtual void __fastcall InsertObject(int Index, const WideString S, System::TObject* AObject);
	virtual void __fastcall LoadFromFile(const WideString FileName);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall SaveToFile(const WideString FileName);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall SetTextW(const wchar_t * Text);
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property WideString CommaText = {read=GetCommaText, write=SetCommaText};
	__property int Count = {read=GetCount, nodefault};
	__property wchar_t Delimiter = {read=GetDelimiter, write=SetDelimiter, nodefault};
	__property WideString DelimitedText = {read=GetDelimitedText, write=SetDelimitedText};
	__property WideString Names[int Index] = {read=GetName};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property wchar_t QuoteChar = {read=GetQuoteChar, write=SetQuoteChar, nodefault};
	__property WideString Values[WideString Name] = {read=GetValue, write=SetValue};
	__property WideString ValueFromIndex[int Index] = {read=GetValueFromIndex, write=SetValueFromIndex};
	__property wchar_t NameValueSeparator = {read=GetNameValueSeparator, nodefault};
	__property WideString Strings[int Index] = {read=Get, write=Put/*, default*/};
	__property WideString Text = {read=GetTextStr, write=SetTextStr};
	__property _di_IWideStringsAdapter StringsAdapter = {read=FAdapter, write=SetStringsAdapter};
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TWideStrings(void) : Classes::TPersistent() { }
	#pragma option pop
	
};


struct TWideStringItem;
typedef TWideStringItem *PWideStringItem;

#pragma pack(push, 4)
struct TWideStringItem
{
	WideString FString;
	System::TObject* FObject;
} ;
#pragma pack(pop)

typedef TWideStringItem TWideStringItemList[134217728];

typedef TWideStringItem *PWideStringItemList;

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Zfuwidestrings */
using namespace Zfuwidestrings;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZFUWideStrings
