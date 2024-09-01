// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCUndo.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncundoHPP
#define Vcl_TmsfncundoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfncundo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCUndoStackItem;
class DELPHICLASS TTMSFNCUndoManager;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCUndoStackItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FActionName;
	System::Classes::TStream* FState;
	System::TObject* FObj;
	
public:
	__fastcall virtual TTMSFNCUndoStackItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCUndoStackItem();
	__property System::UnicodeString ActionName = {read=FActionName, write=FActionName};
	__property System::TObject* Obj = {read=FObj, write=FObj};
	__property System::Classes::TStream* State = {read=FState};
};


class PASCALIMPLEMENTATION TTMSFNCUndoManager : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TTMSFNCUndoStackItem* operator[](int Index) { return this->Stack[Index]; }
	
private:
	int FCurrent;
	System::TObject* FObject;
	int FMaxStackCount;
	TTMSFNCUndoStackItem* __fastcall GetStackItem(int Index);
	
public:
	System::UnicodeString __fastcall NextUndoAction();
	System::UnicodeString __fastcall NextRedoAction();
	bool __fastcall CanUndo();
	bool __fastcall CanRedo();
	void __fastcall Undo();
	void __fastcall Redo();
	void __fastcall ClearUndoStack();
	void __fastcall PushState(const System::UnicodeString AActionName);
	__fastcall TTMSFNCUndoManager(System::TObject* AObject);
	__property int MaxStackCount = {read=FMaxStackCount, write=FMaxStackCount, default=20};
	__property TTMSFNCUndoStackItem* Stack[int Index] = {read=GetStackItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCUndoManager() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncundo */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCUNDO)
using namespace Vcl::Tmsfncundo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncundoHPP
