// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCURLBitmapContainer.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncurlbitmapcontainerHPP
#define Vcl_TmsfncurlbitmapcontainerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCURLBitmapItem : public Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapItem
{
	typedef Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapItem inherited;
	
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
	/* TTMSFNCBitmapItem.Create */ inline __fastcall virtual TTMSFNCURLBitmapItem(System::Classes::TCollection* Collection) : Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapItem(Collection) { }
	/* TTMSFNCBitmapItem.Destroy */ inline __fastcall virtual ~TTMSFNCURLBitmapItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCURLBitmapCollection : public Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection
{
	typedef Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection inherited;
	
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
	/* TTMSFNCBitmapCollection.Create */ inline __fastcall TTMSFNCURLBitmapCollection(Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* AOwner) : Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection(AOwner) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCURLBitmapCollection() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TTMSFNCURLBitmapContainerDownloadCompleteEvent)(System::TObject* Sender, int ItemIndex);

typedef void __fastcall (__closure *TTMSFNCURLBitmapContainerDownloadProgressEvent)(System::TObject* Sender, int ItemIndex, __int64 Position, __int64 TotalSize);

class PASCALIMPLEMENTATION TTMSFNCURLBitmapContainer : public Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer
{
	typedef Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer inherited;
	
private:
	int FWorkerItem;
	__int64 FWorkerPosition;
	__int64 FWorkerTotalSize;
	TTMSFNCURLDownloadThread* FDownloader;
	void *FNetHandle;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FDefaultBitmap;
	TTMSFNCURLBitmapContainerDownloadCompleteEvent FOnDownloadComplete;
	TTMSFNCURLBitmapContainerDownloadProgressEvent FOnDownloadProgress;
	TTMSFNCURLBitmapContainerDownloadCompleteEvent FOnInternalDownloadComplete;
	void __fastcall SetDefaultBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	HIDESBASE TTMSFNCURLBitmapCollection* __fastcall GetItems();
	HIDESBASE void __fastcall SetItems(TTMSFNCURLBitmapCollection* const Value);
	void __fastcall DoProgress();
	void __fastcall DoComplete();
	void __fastcall ThreadTerminated(System::TObject* Sender);
	
protected:
	virtual Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapCollection* __fastcall CreateItems();
	virtual void __fastcall DoDownloadComplete(int ItemIndex);
	virtual void __fastcall DoDownloadProgress(int ItemIndex, __int64 Position, __int64 TotalSize);
	virtual void __fastcall RegisterRuntimeClasses();
	__property TTMSFNCURLBitmapContainerDownloadCompleteEvent OnInternalDownloadComplete = {read=FOnInternalDownloadComplete, write=FOnInternalDownloadComplete};
	
public:
	__fastcall virtual TTMSFNCURLBitmapContainer(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TTMSFNCURLBitmapContainer();
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall FindBitmap(System::UnicodeString s)/* overload */;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall FindBitmapByURL(System::UnicodeString AURL);
	System::Classes::TMemoryStream* __fastcall Download(System::UnicodeString URL);
	virtual void __fastcall AddFromURL(System::UnicodeString URL, System::UnicodeString BitmapName);
	
__published:
	__property TTMSFNCURLBitmapCollection* Items = {read=GetItems, write=SetItems};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* DefaultBitmap = {read=FDefaultBitmap, write=SetDefaultBitmap};
	__property TTMSFNCURLBitmapContainerDownloadCompleteEvent OnDownloadComplete = {read=FOnDownloadComplete, write=FOnDownloadComplete};
	__property TTMSFNCURLBitmapContainerDownloadProgressEvent OnDownloadProgress = {read=FOnDownloadProgress, write=FOnDownloadProgress};
public:
	/* TTMSFNCBitmapContainer.Create */ inline __fastcall virtual TTMSFNCURLBitmapContainer()/* overload */ : Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCURLBitmapContainer(HWND ParentWindow) : Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer(ParentWindow) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  FindBitmap(int i, Vcl::Tmsfnctypes::TTMSFNCBitmap* ABitmap){ Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer::FindBitmap(i, ABitmap); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncurlbitmapcontainer */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCURLBITMAPCONTAINER)
using namespace Vcl::Tmsfncurlbitmapcontainer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncurlbitmapcontainerHPP
