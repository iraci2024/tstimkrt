// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCURLBitmapContainer.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncurlbitmapcontainerHPP
#define Fmx_TmsfncurlbitmapcontainerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncurlbitmapcontainer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCURLDownloadThread;
class DELPHICLASS TTMSFNCURLBitmapItem;
class DELPHICLASS TTMSFNCURLBitmapCollection;
class DELPHICLASS TTMSFNCURLBitmapContainer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCURLDownloadThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TTMSFNCURLBitmapContainer* FBitmapContainer;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TTMSFNCURLDownloadThread(TTMSFNCURLBitmapContainer* ABitmapContainer);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TTMSFNCURLDownloadThread() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCURLBitmapItem : public Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem
{
	typedef Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem inherited;
	
private:
	System::UnicodeString FURL;
	bool FLoaded;
	void __fastcall SetURL(const System::UnicodeString Value);
	
public:
	__property bool Loaded = {read=FLoaded, write=FLoaded, nodefault};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::UnicodeString URL = {read=FURL, write=SetURL};
public:
	/* TTMSFNCBitmapItem.Create */ inline __fastcall virtual TTMSFNCURLBitmapItem(System::Classes::TCollection* Collection) : Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem(Collection) { }
	/* TTMSFNCBitmapItem.Destroy */ inline __fastcall virtual ~TTMSFNCURLBitmapItem() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCURLBitmapCollection : public Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection
{
	typedef Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection inherited;
	
public:
	TTMSFNCURLBitmapItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TTMSFNCURLBitmapItem* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TTMSFNCURLBitmapItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetBitmapItemClass();
	
public:
	HIDESBASE TTMSFNCURLBitmapItem* __fastcall Add();
	HIDESBASE TTMSFNCURLBitmapItem* __fastcall Insert(int index);
	__property TTMSFNCURLBitmapItem* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TTMSFNCBitmapCollection.Create */ inline __fastcall TTMSFNCURLBitmapCollection(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* AOwner) : Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection(AOwner) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCURLBitmapCollection() { }
	
};


typedef void __fastcall (__closure *TTMSFNCURLBitmapContainerDownloadCompleteEvent)(System::TObject* Sender, int ItemIndex);

typedef void __fastcall (__closure *TTMSFNCURLBitmapContainerDownloadProgressEvent)(System::TObject* Sender, int ItemIndex, __int64 Position, __int64 TotalSize);

class PASCALIMPLEMENTATION TTMSFNCURLBitmapContainer : public Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer
{
	typedef Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer inherited;
	
private:
	int FWorkerItem;
	__int64 FWorkerPosition;
	__int64 FWorkerTotalSize;
	TTMSFNCURLDownloadThread* FDownloader;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FDefaultBitmap;
	TTMSFNCURLBitmapContainerDownloadCompleteEvent FOnDownloadComplete;
	TTMSFNCURLBitmapContainerDownloadProgressEvent FOnDownloadProgress;
	TTMSFNCURLBitmapContainerDownloadCompleteEvent FOnInternalDownloadComplete;
	void __fastcall SetDefaultBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	HIDESBASE TTMSFNCURLBitmapCollection* __fastcall GetItems();
	HIDESBASE void __fastcall SetItems(TTMSFNCURLBitmapCollection* const Value);
	void __fastcall DoProgress();
	void __fastcall DoComplete();
	void __fastcall ThreadTerminated(System::TObject* Sender);
	
protected:
	virtual Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection* __fastcall CreateItems();
	virtual void __fastcall DoDownloadComplete(int ItemIndex);
	virtual void __fastcall DoDownloadProgress(int ItemIndex, __int64 Position, __int64 TotalSize);
	virtual void __fastcall RegisterRuntimeClasses();
	__property TTMSFNCURLBitmapContainerDownloadCompleteEvent OnInternalDownloadComplete = {read=FOnInternalDownloadComplete, write=FOnInternalDownloadComplete};
	
public:
	__fastcall virtual TTMSFNCURLBitmapContainer(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCURLBitmapContainer();
	virtual Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall FindBitmap(System::UnicodeString s)/* overload */;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall FindBitmapByURL(System::UnicodeString AURL);
	System::Classes::TMemoryStream* __fastcall Download(System::UnicodeString URL);
	virtual void __fastcall AddFromURL(System::UnicodeString URL, System::UnicodeString BitmapName);
	
__published:
	__property TTMSFNCURLBitmapCollection* Items = {read=GetItems, write=SetItems};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* DefaultBitmap = {read=FDefaultBitmap, write=SetDefaultBitmap};
	__property TTMSFNCURLBitmapContainerDownloadCompleteEvent OnDownloadComplete = {read=FOnDownloadComplete, write=FOnDownloadComplete};
	__property TTMSFNCURLBitmapContainerDownloadProgressEvent OnDownloadProgress = {read=FOnDownloadProgress, write=FOnDownloadProgress};
public:
	/* TTMSFNCBitmapContainer.Create */ inline __fastcall virtual TTMSFNCURLBitmapContainer()/* overload */ : Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  FindBitmap(int i, Fmx::Tmsfnctypes::TTMSFNCBitmap* ABitmap){ Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer::FindBitmap(i, ABitmap); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncurlbitmapcontainer */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCURLBITMAPCONTAINER)
using namespace Fmx::Tmsfncurlbitmapcontainer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncurlbitmapcontainerHPP
