// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPrinters.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncprintersHPP
#define Fmx_TmsfncprintersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Printer.hpp>
#include <System.UITypes.hpp>
#include <FMX.Helpers.Android.hpp>
#include <Androidapi.JNI.JavaTypes.hpp>
#include <Androidapi.JNIBridge.hpp>
#include <Androidapi.Helpers.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncprinters
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE JPrintManagerClass;
typedef System::DelphiInterface<JPrintManagerClass> _di_JPrintManagerClass;
__interface DELPHIINTERFACE JPrintManager;
typedef System::DelphiInterface<JPrintManager> _di_JPrintManager;
class DELPHICLASS TJPrintManager;
__interface DELPHIINTERFACE JPrintJobClass;
typedef System::DelphiInterface<JPrintJobClass> _di_JPrintJobClass;
__interface DELPHIINTERFACE JPrintJob;
typedef System::DelphiInterface<JPrintJob> _di_JPrintJob;
class DELPHICLASS TJPrintJob;
__interface DELPHIINTERFACE JPrintDocumentAdapterClass;
typedef System::DelphiInterface<JPrintDocumentAdapterClass> _di_JPrintDocumentAdapterClass;
__interface DELPHIINTERFACE JPrintDocumentAdapter;
typedef System::DelphiInterface<JPrintDocumentAdapter> _di_JPrintDocumentAdapter;
class DELPHICLASS TJPrintDocumentAdapter;
__interface DELPHIINTERFACE JFNCPrintDocumentAdapterClass;
typedef System::DelphiInterface<JFNCPrintDocumentAdapterClass> _di_JFNCPrintDocumentAdapterClass;
__interface DELPHIINTERFACE JFNCPrintDocumentAdapter;
typedef System::DelphiInterface<JFNCPrintDocumentAdapter> _di_JFNCPrintDocumentAdapter;
class DELPHICLASS TJFNCPrintDocumentAdapter;
__interface DELPHIINTERFACE JFNCPrintDocumentAdapterListenerClass;
typedef System::DelphiInterface<JFNCPrintDocumentAdapterListenerClass> _di_JFNCPrintDocumentAdapterListenerClass;
__interface DELPHIINTERFACE JFNCPrintDocumentAdapterListener;
typedef System::DelphiInterface<JFNCPrintDocumentAdapterListener> _di_JFNCPrintDocumentAdapterListener;
class DELPHICLASS TFNCPrintDocumentAdapterListener;
class DELPHICLASS TTMSFNCPrintDocumentAdapterListener;
__interface DELPHIINTERFACE JPrintAttributes_BuilderClass;
typedef System::DelphiInterface<JPrintAttributes_BuilderClass> _di_JPrintAttributes_BuilderClass;
__interface DELPHIINTERFACE JPrintAttributes_Builder;
typedef System::DelphiInterface<JPrintAttributes_Builder> _di_JPrintAttributes_Builder;
class DELPHICLASS TJPrintAttributes_Builder;
__interface DELPHIINTERFACE JPrintAttributesClass;
typedef System::DelphiInterface<JPrintAttributesClass> _di_JPrintAttributesClass;
__interface DELPHIINTERFACE JPrintAttributes;
typedef System::DelphiInterface<JPrintAttributes> _di_JPrintAttributes;
class DELPHICLASS TJPrintAttributes;
__interface DELPHIINTERFACE JPrintAttributes_ResolutionClass;
typedef System::DelphiInterface<JPrintAttributes_ResolutionClass> _di_JPrintAttributes_ResolutionClass;
__interface DELPHIINTERFACE JPrintAttributes_Resolution;
typedef System::DelphiInterface<JPrintAttributes_Resolution> _di_JPrintAttributes_Resolution;
class DELPHICLASS TJPrintAttributes_Resolution;
__interface DELPHIINTERFACE JPrintAttributes_MarginsClass;
typedef System::DelphiInterface<JPrintAttributes_MarginsClass> _di_JPrintAttributes_MarginsClass;
__interface DELPHIINTERFACE JPrintAttributes_Margins;
typedef System::DelphiInterface<JPrintAttributes_Margins> _di_JPrintAttributes_Margins;
class DELPHICLASS TJPrintAttributes_Margins;
__interface DELPHIINTERFACE JPrintAttributes_MediaSizeClass;
typedef System::DelphiInterface<JPrintAttributes_MediaSizeClass> _di_JPrintAttributes_MediaSizeClass;
__interface DELPHIINTERFACE JPrintAttributes_MediaSize;
typedef System::DelphiInterface<JPrintAttributes_MediaSize> _di_JPrintAttributes_MediaSize;
class DELPHICLASS TJPrintAttributes_MediaSize;
__interface DELPHIINTERFACE TTMSFNCPrinterDrawContentCallBack;
typedef System::DelphiInterface<TTMSFNCPrinterDrawContentCallBack> _di_TTMSFNCPrinterDrawContentCallBack;
class DELPHICLASS TTMSFNCPrinter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TPrintSize : unsigned char { psISO_A0, psISO_A1, psISO_A10, psISO_A2, psISO_A3, psISO_A4, psISO_A5, psISO_A6, psISO_A7, psISO_A8, psISO_A9, psISO_B0, psISO_B1, psISO_B10, psISO_B2, psISO_B3, psISO_B4, psISO_B5, psISO_B6, psISO_B7, psISO_B8, psISO_B9, psISO_C0, psISO_C1, psISO_C10, psISO_C2, psISO_C3, psISO_C4, psISO_C5, psISO_C6, psISO_C7, psISO_C8, psISO_C9, psJIS_B0, psJIS_B1, psJIS_B10, psJIS_B2, psJIS_B3, psJIS_B4, psJIS_B5, psJIS_B6, psJIS_B7, psJIS_B8, psJIS_B9, psJIS_EXEC, psJPN_CHOU2, psJPN_CHOU3, psJPN_CHOU4, psJPN_HAGAKI, psJPN_KAHU, psJPN_KAKU2, psJPN_OUFUKU, psJPN_YOU4, psNA_FOOLSCAP, psNA_GOVT_LETTER, psNA_INDEX_3X5, psNA_INDEX_4X6, psNA_INDEX_5X8, psNA_JUNIOR_LEGAL, psNA_LEDGER, psNA_LEGAL, psNA_LETTER, psNA_MONARCH, 
	psNA_QUARTO, psNA_TABLOID, psOM_DAI_PA_KAI, psOM_JUURO_KU_KAI, psOM_PA_KAI, psPRC_1, psPRC_10, psPRC_16K, psPRC_2, psPRC_3, psPRC_4, psPRC_5, psPRC_6, psPRC_7, psPRC_8, psPRC_9, psROC_16K, psROC_8K, psUNKNOWN_LANDSCAPE, psUNKNOWN_PORTRAIT };

__interface  INTERFACE_UUID("{7F56A1C8-FC5E-435C-930A-5EAC1D1E8571}") JPrintManagerClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{8CF65E90-1E7F-4658-B0B9-4FB40114EB78}") JPrintManager  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual Androidapi::Jni::Javatypes::_di_JList __cdecl getPrintJobs() = 0 ;
	virtual _di_JPrintJob __fastcall print(Androidapi::Jni::Javatypes::_di_JString printJobName, _di_JPrintDocumentAdapter documentAdapter, _di_JPrintAttributes PrintAttributes) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintManager : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintManagerClass,_di_JPrintManager>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintManagerClass,_di_JPrintManager> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintManager() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintManagerClass,_di_JPrintManager>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintManager() { }
	
};

#pragma pack(pop)

__interface JPrintJobClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{8A914568-3CBC-45CA-9783-4590410352E2}") JPrintJob  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual void __cdecl cancel() = 0 ;
	HIDESBASE virtual bool __cdecl equals(Androidapi::Jni::Javatypes::_di_JObject obj) = 0 ;
	HIDESBASE virtual int __cdecl hashCode() = 0 ;
	virtual bool __cdecl isBlocked() = 0 ;
	virtual bool __cdecl isCancelled() = 0 ;
	virtual bool __cdecl isCompleted() = 0 ;
	virtual bool __cdecl isFailed() = 0 ;
	virtual bool __cdecl isQueued() = 0 ;
	virtual bool __cdecl isStarted() = 0 ;
	virtual void __cdecl restart() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintJob : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintJobClass,_di_JPrintJob>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintJobClass,_di_JPrintJob> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintJob() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintJobClass,_di_JPrintJob>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintJob() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{9E14C8BC-810E-4BEA-9635-AA79DA6D3476}") JPrintDocumentAdapterClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl _GetEXTRA_PRINT_PREVIEW() = 0 ;
	HIDESBASE virtual _di_JPrintDocumentAdapter __cdecl init() = 0 ;
	__property Androidapi::Jni::Javatypes::_di_JString EXTRA_PRINT_PREVIEW = {read=_GetEXTRA_PRINT_PREVIEW};
};

__interface  INTERFACE_UUID("{CE4783E1-D54F-46FA-B2DC-DB65A20DC786}") JPrintDocumentAdapter  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual void __cdecl onFinish() = 0 ;
	virtual void __cdecl onStart() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintDocumentAdapter : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintDocumentAdapterClass,_di_JPrintDocumentAdapter>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintDocumentAdapterClass,_di_JPrintDocumentAdapter> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintDocumentAdapter() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintDocumentAdapterClass,_di_JPrintDocumentAdapter>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintDocumentAdapter() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{2367BEE9-732E-4BA4-B8FD-863B4028F7DB}") JFNCPrintDocumentAdapterClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JFNCPrintDocumentAdapter __cdecl init(Androidapi::Jni::Graphicscontentviewtext::_di_JContext context) = 0 ;
};

__interface  INTERFACE_UUID("{3C06CA10-5314-4A11-8584-600BC2879EF0}") JFNCPrintDocumentAdapter  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual void __cdecl setListener(_di_JFNCPrintDocumentAdapterListener l) = 0 ;
	virtual void __cdecl setPageHeight(int h) = 0 ;
	virtual void __cdecl setPageWidth(int w) = 0 ;
	virtual void __cdecl setPrintName(Androidapi::Jni::Javatypes::_di_JString jn) = 0 ;
	virtual void __cdecl setDPI(int d) = 0 ;
	virtual void __cdecl setMyPrintAttributes(_di_JPrintAttributes pa) = 0 ;
	virtual void __cdecl newPage() = 0 ;
	virtual void __cdecl endDoc() = 0 ;
	virtual Androidapi::Jni::Graphicscontentviewtext::_di_JCanvas __cdecl getMyPageCanvas() = 0 ;
	virtual int __cdecl getPageHeight() = 0 ;
	virtual int __cdecl getPageWidth() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getPrintName() = 0 ;
	virtual int __cdecl getPageNumber() = 0 ;
	virtual int __cdecl getDPI() = 0 ;
	virtual _di_JPrintAttributes __cdecl getMyPrintAttributes() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJFNCPrintDocumentAdapter : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterClass,_di_JFNCPrintDocumentAdapter>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterClass,_di_JFNCPrintDocumentAdapter> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJFNCPrintDocumentAdapter() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterClass,_di_JFNCPrintDocumentAdapter>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJFNCPrintDocumentAdapter() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{CF3D751A-EEF4-4BCF-B4F5-C766EC76B3DA}") JFNCPrintDocumentAdapterListenerClass  : public Androidapi::Jnibridge::IJavaClass 
{
	
};

__interface  INTERFACE_UUID("{148558C6-ACFA-4BE7-A8BA-3C2F8F66A1BF}") JFNCPrintDocumentAdapterListener  : public Androidapi::Jnibridge::IJavaInstance 
{
	virtual void __cdecl onWriteContent() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFNCPrintDocumentAdapterListener : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterListenerClass,_di_JFNCPrintDocumentAdapterListener>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterListenerClass,_di_JFNCPrintDocumentAdapterListener> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TFNCPrintDocumentAdapterListener() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JFNCPrintDocumentAdapterListenerClass,_di_JFNCPrintDocumentAdapterListener>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFNCPrintDocumentAdapterListener() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPrintDocumentAdapterListener : public Androidapi::Jnibridge::TJavaLocal
{
	typedef Androidapi::Jnibridge::TJavaLocal inherited;
	
private:
	TTMSFNCPrinter* FPrinter;
	
public:
	void __cdecl onWriteContent();
public:
	/* TJavaLocal.Create */ inline __fastcall TTMSFNCPrintDocumentAdapterListener() : Androidapi::Jnibridge::TJavaLocal() { }
	/* TJavaLocal.Destroy */ inline __fastcall virtual ~TTMSFNCPrintDocumentAdapterListener() { }
	
private:
	void *__JFNCPrintDocumentAdapterListener;	// JFNCPrintDocumentAdapterListener 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {148558C6-ACFA-4BE7-A8BA-3C2F8F66A1BF}
	operator _di_JFNCPrintDocumentAdapterListener()
	{
		_di_JFNCPrintDocumentAdapterListener intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator JFNCPrintDocumentAdapterListener*(void) { return (JFNCPrintDocumentAdapterListener*)&__JFNCPrintDocumentAdapterListener; }
	#endif
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{62D77F93-F2EA-4484-9D7E-7C904EBE8947}") JPrintAttributes_BuilderClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	HIDESBASE virtual _di_JPrintAttributes_Builder __cdecl init() = 0 ;
};

__interface  INTERFACE_UUID("{E4A8CF83-046F-453A-AEB6-6446FF689939}") JPrintAttributes_Builder  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JPrintAttributes __cdecl build() = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setColorMode(int colorMode) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setMediaSize(_di_JPrintAttributes_MediaSize mediaSize) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setMargins(_di_JPrintAttributes_Margins Margins) = 0 ;
	virtual _di_JPrintAttributes_Builder __cdecl setResolution(_di_JPrintAttributes_Resolution Resolution) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Builder : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Builder() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_BuilderClass,_di_JPrintAttributes_Builder>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Builder() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{D9B40228-CC37-4573-896E-10D7800670CE}") JPrintAttributesClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual int __cdecl _GetCOLOR_MODE_COLOR() = 0 ;
	virtual int __cdecl _GetCOLOR_MODE_MONOCHROME() = 0 ;
	virtual int __cdecl _GetDUPLEX_MODE_LONG_EDGE() = 0 ;
	virtual int __cdecl _GetDUPLEX_MODE_NONE() = 0 ;
	virtual int __cdecl _GetDUPLEX_MODE_SHORT_EDGE() = 0 ;
	__property int COLOR_MODE_COLOR = {read=_GetCOLOR_MODE_COLOR};
	__property int COLOR_MODE_MONOCHROME = {read=_GetCOLOR_MODE_MONOCHROME};
	__property int DUPLEX_MODE_LONG_EDGE = {read=_GetDUPLEX_MODE_LONG_EDGE};
	__property int DUPLEX_MODE_NONE = {read=_GetDUPLEX_MODE_NONE};
	__property int DUPLEX_MODE_SHORT_EDGE = {read=_GetDUPLEX_MODE_SHORT_EDGE};
};

__interface  INTERFACE_UUID("{312C7538-43CD-4607-B8E2-04CF431C9B5B}") JPrintAttributes  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual int __cdecl describeContents() = 0 ;
	HIDESBASE virtual bool __cdecl equals(Androidapi::Jni::Javatypes::_di_JObject obj) = 0 ;
	virtual int __cdecl getColorMode() = 0 ;
	virtual int __cdecl getDuplexMode() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl getMediaSize() = 0 ;
	virtual _di_JPrintAttributes_Margins __cdecl getMinMargins() = 0 ;
	virtual _di_JPrintAttributes_Resolution __cdecl getResolution() = 0 ;
	HIDESBASE virtual int __cdecl hashCode() = 0 ;
	HIDESBASE virtual Androidapi::Jni::Javatypes::_di_JString __cdecl toString() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributesClass,_di_JPrintAttributes>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{890A9602-792D-417F-B413-41CD6576A178}") JPrintAttributes_ResolutionClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{0E4C6F96-FB83-4187-B26C-332F9DBDEBD3}") JPrintAttributes_Resolution  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Resolution : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Resolution() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_ResolutionClass,_di_JPrintAttributes_Resolution>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Resolution() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{CB38E8EF-9112-4F1F-B713-0B2E03A8EDF3}") JPrintAttributes_MarginsClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	
};

__interface  INTERFACE_UUID("{21BF97D5-0C39-4F71-BA25-1B1C168BF23E}") JPrintAttributes_Margins  : public Androidapi::Jni::Javatypes::JObject 
{
	
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_Margins : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_Margins() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MarginsClass,_di_JPrintAttributes_Margins>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_Margins() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{0A2D852F-16AE-49A1-BCFE-E1E3E12A15C4}") JPrintAttributes_MediaSizeClass  : public Androidapi::Jni::Javatypes::JObjectClass 
{
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_A9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_B9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetISO_C9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B0() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_B9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJIS_EXEC() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_CHOU4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_HAGAKI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_KAHU() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_KAKU2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_OUFUKU() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetJPN_YOU4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_FOOLSCAP() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_GOVT_LETTER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_3X5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_4X6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_INDEX_5X8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_JUNIOR_LEGAL() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LEDGER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LEGAL() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_LETTER() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_MONARCH() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_QUARTO() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetNA_TABLOID() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_DAI_PA_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_JUURO_KU_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetOM_PA_KAI() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_1() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_10() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_16K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_2() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_3() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_4() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_5() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_6() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_7() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_8() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetPRC_9() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetROC_16K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetROC_8K() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetUNKNOWN_LANDSCAPE() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl _GetUNKNOWN_PORTRAIT() = 0 ;
	HIDESBASE virtual _di_JPrintAttributes_MediaSize __cdecl init(Androidapi::Jni::Javatypes::_di_JString id, Androidapi::Jni::Javatypes::_di_JString label_, int widthMils, int heightMils) = 0 ;
	__property _di_JPrintAttributes_MediaSize ISO_A0 = {read=_GetISO_A0};
	__property _di_JPrintAttributes_MediaSize ISO_A1 = {read=_GetISO_A1};
	__property _di_JPrintAttributes_MediaSize ISO_A10 = {read=_GetISO_A10};
	__property _di_JPrintAttributes_MediaSize ISO_A2 = {read=_GetISO_A2};
	__property _di_JPrintAttributes_MediaSize ISO_A3 = {read=_GetISO_A3};
	__property _di_JPrintAttributes_MediaSize ISO_A4 = {read=_GetISO_A4};
	__property _di_JPrintAttributes_MediaSize ISO_A5 = {read=_GetISO_A5};
	__property _di_JPrintAttributes_MediaSize ISO_A6 = {read=_GetISO_A6};
	__property _di_JPrintAttributes_MediaSize ISO_A7 = {read=_GetISO_A7};
	__property _di_JPrintAttributes_MediaSize ISO_A8 = {read=_GetISO_A8};
	__property _di_JPrintAttributes_MediaSize ISO_A9 = {read=_GetISO_A9};
	__property _di_JPrintAttributes_MediaSize ISO_B0 = {read=_GetISO_B0};
	__property _di_JPrintAttributes_MediaSize ISO_B1 = {read=_GetISO_B1};
	__property _di_JPrintAttributes_MediaSize ISO_B10 = {read=_GetISO_B10};
	__property _di_JPrintAttributes_MediaSize ISO_B2 = {read=_GetISO_B2};
	__property _di_JPrintAttributes_MediaSize ISO_B3 = {read=_GetISO_B3};
	__property _di_JPrintAttributes_MediaSize ISO_B4 = {read=_GetISO_B4};
	__property _di_JPrintAttributes_MediaSize ISO_B5 = {read=_GetISO_B5};
	__property _di_JPrintAttributes_MediaSize ISO_B6 = {read=_GetISO_B6};
	__property _di_JPrintAttributes_MediaSize ISO_B7 = {read=_GetISO_B7};
	__property _di_JPrintAttributes_MediaSize ISO_B8 = {read=_GetISO_B8};
	__property _di_JPrintAttributes_MediaSize ISO_B9 = {read=_GetISO_B9};
	__property _di_JPrintAttributes_MediaSize ISO_C0 = {read=_GetISO_C0};
	__property _di_JPrintAttributes_MediaSize ISO_C1 = {read=_GetISO_C1};
	__property _di_JPrintAttributes_MediaSize ISO_C10 = {read=_GetISO_C10};
	__property _di_JPrintAttributes_MediaSize ISO_C2 = {read=_GetISO_C2};
	__property _di_JPrintAttributes_MediaSize ISO_C3 = {read=_GetISO_C3};
	__property _di_JPrintAttributes_MediaSize ISO_C4 = {read=_GetISO_C4};
	__property _di_JPrintAttributes_MediaSize ISO_C5 = {read=_GetISO_C5};
	__property _di_JPrintAttributes_MediaSize ISO_C6 = {read=_GetISO_C6};
	__property _di_JPrintAttributes_MediaSize ISO_C7 = {read=_GetISO_C7};
	__property _di_JPrintAttributes_MediaSize ISO_C8 = {read=_GetISO_C8};
	__property _di_JPrintAttributes_MediaSize ISO_C9 = {read=_GetISO_C9};
	__property _di_JPrintAttributes_MediaSize JIS_B0 = {read=_GetJIS_B0};
	__property _di_JPrintAttributes_MediaSize JIS_B1 = {read=_GetJIS_B1};
	__property _di_JPrintAttributes_MediaSize JIS_B10 = {read=_GetJIS_B10};
	__property _di_JPrintAttributes_MediaSize JIS_B2 = {read=_GetJIS_B2};
	__property _di_JPrintAttributes_MediaSize JIS_B3 = {read=_GetJIS_B3};
	__property _di_JPrintAttributes_MediaSize JIS_B4 = {read=_GetJIS_B4};
	__property _di_JPrintAttributes_MediaSize JIS_B5 = {read=_GetJIS_B5};
	__property _di_JPrintAttributes_MediaSize JIS_B6 = {read=_GetJIS_B6};
	__property _di_JPrintAttributes_MediaSize JIS_B7 = {read=_GetJIS_B7};
	__property _di_JPrintAttributes_MediaSize JIS_B8 = {read=_GetJIS_B8};
	__property _di_JPrintAttributes_MediaSize JIS_B9 = {read=_GetJIS_B9};
	__property _di_JPrintAttributes_MediaSize JIS_EXEC = {read=_GetJIS_EXEC};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU2 = {read=_GetJPN_CHOU2};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU3 = {read=_GetJPN_CHOU3};
	__property _di_JPrintAttributes_MediaSize JPN_CHOU4 = {read=_GetJPN_CHOU4};
	__property _di_JPrintAttributes_MediaSize JPN_HAGAKI = {read=_GetJPN_HAGAKI};
	__property _di_JPrintAttributes_MediaSize JPN_KAHU = {read=_GetJPN_KAHU};
	__property _di_JPrintAttributes_MediaSize JPN_KAKU2 = {read=_GetJPN_KAKU2};
	__property _di_JPrintAttributes_MediaSize JPN_OUFUKU = {read=_GetJPN_OUFUKU};
	__property _di_JPrintAttributes_MediaSize JPN_YOU4 = {read=_GetJPN_YOU4};
	__property _di_JPrintAttributes_MediaSize NA_FOOLSCAP = {read=_GetNA_FOOLSCAP};
	__property _di_JPrintAttributes_MediaSize NA_GOVT_LETTER = {read=_GetNA_GOVT_LETTER};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_3X5 = {read=_GetNA_INDEX_3X5};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_4X6 = {read=_GetNA_INDEX_4X6};
	__property _di_JPrintAttributes_MediaSize NA_INDEX_5X8 = {read=_GetNA_INDEX_5X8};
	__property _di_JPrintAttributes_MediaSize NA_JUNIOR_LEGAL = {read=_GetNA_JUNIOR_LEGAL};
	__property _di_JPrintAttributes_MediaSize NA_LEDGER = {read=_GetNA_LEDGER};
	__property _di_JPrintAttributes_MediaSize NA_LEGAL = {read=_GetNA_LEGAL};
	__property _di_JPrintAttributes_MediaSize NA_LETTER = {read=_GetNA_LETTER};
	__property _di_JPrintAttributes_MediaSize NA_MONARCH = {read=_GetNA_MONARCH};
	__property _di_JPrintAttributes_MediaSize NA_QUARTO = {read=_GetNA_QUARTO};
	__property _di_JPrintAttributes_MediaSize NA_TABLOID = {read=_GetNA_TABLOID};
	__property _di_JPrintAttributes_MediaSize OM_DAI_PA_KAI = {read=_GetOM_DAI_PA_KAI};
	__property _di_JPrintAttributes_MediaSize OM_JUURO_KU_KAI = {read=_GetOM_JUURO_KU_KAI};
	__property _di_JPrintAttributes_MediaSize OM_PA_KAI = {read=_GetOM_PA_KAI};
	__property _di_JPrintAttributes_MediaSize PRC_1 = {read=_GetPRC_1};
	__property _di_JPrintAttributes_MediaSize PRC_10 = {read=_GetPRC_10};
	__property _di_JPrintAttributes_MediaSize PRC_16K = {read=_GetPRC_16K};
	__property _di_JPrintAttributes_MediaSize PRC_2 = {read=_GetPRC_2};
	__property _di_JPrintAttributes_MediaSize PRC_3 = {read=_GetPRC_3};
	__property _di_JPrintAttributes_MediaSize PRC_4 = {read=_GetPRC_4};
	__property _di_JPrintAttributes_MediaSize PRC_5 = {read=_GetPRC_5};
	__property _di_JPrintAttributes_MediaSize PRC_6 = {read=_GetPRC_6};
	__property _di_JPrintAttributes_MediaSize PRC_7 = {read=_GetPRC_7};
	__property _di_JPrintAttributes_MediaSize PRC_8 = {read=_GetPRC_8};
	__property _di_JPrintAttributes_MediaSize PRC_9 = {read=_GetPRC_9};
	__property _di_JPrintAttributes_MediaSize ROC_16K = {read=_GetROC_16K};
	__property _di_JPrintAttributes_MediaSize ROC_8K = {read=_GetROC_8K};
	__property _di_JPrintAttributes_MediaSize UNKNOWN_LANDSCAPE = {read=_GetUNKNOWN_LANDSCAPE};
	__property _di_JPrintAttributes_MediaSize UNKNOWN_PORTRAIT = {read=_GetUNKNOWN_PORTRAIT};
};

__interface  INTERFACE_UUID("{1F95EC3E-8188-4438-8575-C44CABB52DE5}") JPrintAttributes_MediaSize  : public Androidapi::Jni::Javatypes::JObject 
{
	virtual _di_JPrintAttributes_MediaSize __cdecl asLandscape() = 0 ;
	virtual _di_JPrintAttributes_MediaSize __cdecl asPortrait() = 0 ;
	HIDESBASE virtual bool __cdecl equals(Androidapi::Jni::Javatypes::_di_JObject obj) = 0 ;
	virtual int __cdecl getHeightMils() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getId() = 0 ;
	virtual Androidapi::Jni::Javatypes::_di_JString __cdecl getLabel(Androidapi::Jni::Graphicscontentviewtext::_di_JPackageManager packageManager) = 0 ;
	virtual int __cdecl getWidthMils() = 0 ;
	HIDESBASE virtual int __cdecl hashCode() = 0 ;
	virtual bool __cdecl isPortrait() = 0 ;
	HIDESBASE virtual Androidapi::Jni::Javatypes::_di_JString __cdecl toString() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJPrintAttributes_MediaSize : public Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize>
{
	typedef Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize> inherited;
	
public:
	/* TObject.Create */ inline __fastcall TJPrintAttributes_MediaSize() : Androidapi::Jnibridge::TJavaGenericImport__2<_di_JPrintAttributes_MediaSizeClass,_di_JPrintAttributes_MediaSize>() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJPrintAttributes_MediaSize() { }
	
};

#pragma pack(pop)

__interface TTMSFNCPrinterDrawContentCallBack  : public System::IInterface 
{
	virtual void __fastcall Invoke() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPrinter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FDPI;
	System::Uitypes::TPrinterOrientation FOrientation;
	int FPageHeight;
	int FPageNumber;
	int FPageWidth;
	_di_JFNCPrintDocumentAdapter mp;
	_di_JPrintAttributes pa;
	TTMSFNCPrintDocumentAdapterListener* pdaListener;
	_di_JPrintManager pm;
	TPrintSize FPrintSize;
	Fmx::Tmsfncgraphics::TTMSFNCGraphics* FGraphics;
	_di_TTMSFNCPrinterDrawContentCallBack FOnDrawContent;
	int __fastcall GetDPI();
	System::UnicodeString __fastcall GetPrintJobName();
	void __fastcall SetPrintAttributes();
	void __fastcall SetPrintJobName(const System::UnicodeString Value);
	void __fastcall setPrintSize(const TPrintSize Value);
	void __fastcall SetDPI(const int Value);
	System::UnicodeString __fastcall GetDevice();
	System::Uitypes::TPrinterOrientation __fastcall GetOrientation();
	int __fastcall GetPageHeight();
	int __fastcall GetPageNumber();
	int __fastcall GetPageWidth();
	void __fastcall SetOrientation(const System::Uitypes::TPrinterOrientation Value);
	
public:
	__fastcall virtual TTMSFNCPrinter();
	__fastcall virtual ~TTMSFNCPrinter();
	void __fastcall BeginDoc();
	void __fastcall EndDoc();
	void __fastcall NewPage();
	__property System::UnicodeString Device = {read=GetDevice};
	__property int DPI = {read=GetDPI, write=SetDPI, nodefault};
	__property Fmx::Tmsfncgraphics::TTMSFNCGraphics* Graphics = {read=FGraphics};
	__property System::Uitypes::TPrinterOrientation Orientation = {read=GetOrientation, write=SetOrientation, nodefault};
	__property int PageHeight = {read=GetPageHeight, nodefault};
	__property int PageNumber = {read=GetPageNumber, nodefault};
	__property int PageWidth = {read=GetPageWidth, nodefault};
	__property System::UnicodeString PrintJobName = {read=GetPrintJobName, write=SetPrintJobName};
	__property TPrintSize PrintSize = {read=FPrintSize, write=setPrintSize, nodefault};
	__property _di_TTMSFNCPrinterDrawContentCallBack OnDrawContent = {read=FOnDrawContent, write=FOnDrawContent};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x2);
static constexpr System::Int8 PRINTDPI = System::Int8(0x48);
extern DELPHI_PACKAGE TTMSFNCPrinter* __fastcall TMSFNCPrinter(void);
}	/* namespace Tmsfncprinters */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPRINTERS)
using namespace Fmx::Tmsfncprinters;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncprintersHPP
