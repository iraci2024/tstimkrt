// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCloudBase.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccloudbaseHPP
#define Fmx_TmsfnccloudbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Threading.hpp>
#include <System.Generics.Defaults.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccloudbase
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE TTMSFNCCloudBaseRequestResultStringEvent;
typedef System::DelphiInterface<TTMSFNCCloudBaseRequestResultStringEvent> _di_TTMSFNCCloudBaseRequestResultStringEvent;
__interface DELPHIINTERFACE TTMSFNCCloudBaseRequestResultStreamEvent;
typedef System::DelphiInterface<TTMSFNCCloudBaseRequestResultStreamEvent> _di_TTMSFNCCloudBaseRequestResultStreamEvent;
__interface DELPHIINTERFACE TTMSFNCCloudBaseRequestResultEvent;
typedef System::DelphiInterface<TTMSFNCCloudBaseRequestResultEvent> _di_TTMSFNCCloudBaseRequestResultEvent;
class DELPHICLASS TTMSFNCCloudBaseRequestResultEventWrapper;
__interface DELPHIINTERFACE TTMSFNCCloudBaseRequestResultProgressEvent;
typedef System::DelphiInterface<TTMSFNCCloudBaseRequestResultProgressEvent> _di_TTMSFNCCloudBaseRequestResultProgressEvent;
__interface DELPHIINTERFACE ITMSFNCCustomCloudBase;
typedef System::DelphiInterface<ITMSFNCCustomCloudBase> _di_ITMSFNCCustomCloudBase;
__interface DELPHIINTERFACE ITMSFNCCloudBaseService;
typedef System::DelphiInterface<ITMSFNCCloudBaseService> _di_ITMSFNCCloudBaseService;
class DELPHICLASS TTMSFNCCloudBaseRequestHeader;
class DELPHICLASS TTMSFNCCloudBaseRequestHeaders;
class DELPHICLASS TTMSFNCCloudBaseRequests;
class DELPHICLASS TTMSFNCCloudBaseRequestCurlParserFile;
class DELPHICLASS TTMSFNCCloudBaseRequestCurlParser;
class DELPHICLASS TTMSFNCCloudBaseRequestPostDataBuilder;
class DELPHICLASS TTMSFNCCloudBaseRequest;
class DELPHICLASS TTMSFNCCloudBaseRequestResult;
class DELPHICLASS TTMSFNCCloudBaseService;
class DELPHICLASS TTMSFNCCustomCloudBase;
class DELPHICLASS TTMSFNCCloudBase;
class DELPHICLASS TTMSFNCCloudBaseList;
class DELPHICLASS TTMSFNCCloudBaseFactoryService;
class DELPHICLASS TTMSFNCCloudPlatformServicesService;
class DELPHICLASS TTMSFNCCloudPlatformServicesList;
class DELPHICLASS TTMSFNCCloudBasePlatformServices;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCCloudBaseLocale : unsigned char { cblDefault, cblEnglish, cblDutch, cblGerman, cblFrench, cblSpanish, cblItalian, cblPortuguese, cblGreek, cblDanish, cblRussian, cblRomanian, cblSwedish, cblFinnish, cblTurkish, cblJapanese };

typedef void __fastcall (__closure *TTMSFNCCloudBaseRequestEvent)(TTMSFNCCloudBaseRequestResult* const ARequest);

typedef void __fastcall (__closure *TTMSFNCCloudBaseRequestCompleteEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequest);

typedef void __fastcall (__closure *TTMSFNCCloudBaseRequestRunEvent)(TTMSFNCCloudBaseRequestResult* const ARequest);

__interface TTMSFNCCloudBaseRequestResultStringEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(const System::UnicodeString AResult) = 0 ;
};

__interface TTMSFNCCloudBaseRequestResultStreamEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(System::Classes::TMemoryStream* const AResult) = 0 ;
};

__interface TTMSFNCCloudBaseRequestResultEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(TTMSFNCCloudBaseRequestResult* const ARequestResult) = 0 ;
};

class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestResultEventWrapper : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	_di_TTMSFNCCloudBaseRequestResultEvent FCallbackRequest;
	_di_TTMSFNCCloudBaseRequestResultStringEvent FCallbackString;
	_di_TTMSFNCCloudBaseRequestResultStreamEvent FCallbackStream;
	
public:
	__fastcall TTMSFNCCloudBaseRequestResultEventWrapper(_di_TTMSFNCCloudBaseRequestResultEvent ACallbackRequest, _di_TTMSFNCCloudBaseRequestResultStringEvent ACallbackString, _di_TTMSFNCCloudBaseRequestResultStreamEvent ACallbackStream);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequestResultEventWrapper() { }
	
};


__interface TTMSFNCCloudBaseRequestResultProgressEvent  : public System::IInterface 
{
	virtual void __fastcall Invoke(TTMSFNCCloudBaseRequestResult* const ARequestResult, float AProgress) = 0 ;
};

typedef void __fastcall (__closure *TTMSFNCCloudBaseLogEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequestResult, System::UnicodeString AMessage);

typedef void __fastcall (__closure *TTMSFNCCloudBaseRequestLogEvent)(TTMSFNCCloudBaseRequestResult* const ARequestResult, System::UnicodeString AMessage);

typedef void __fastcall (__closure *TTMSFNCCloudBaseProgressEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequestResult, float AProgress, bool AUpload);

typedef void __fastcall (__closure *TTMSFNCCloudBaseRequestProgressEvent)(TTMSFNCCloudBaseRequestResult* const ARequestResult, float AProgress, bool AUpload);

typedef void __fastcall (__closure *TTMSFNCCloudBaseCompleteEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequestResult);

typedef void __fastcall (__closure *TTMSFNCCloudBaseCancelledEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequestResult);

typedef void __fastcall (__closure *TTMSFNCCloudBaseStartedEvent)(System::TObject* Sender, TTMSFNCCloudBaseRequestResult* const ARequestResult);

typedef void __fastcall (__closure *TTMSFNCCloudBaseExternalBrowserCallBackEvent)(const System::UnicodeString ACallbackURLResult);

__interface  INTERFACE_UUID("{F5D2BC35-639A-4EBF-8201-06B3F9EFB82D}") ITMSFNCCustomCloudBase  : public System::IInterface 
{
	virtual __int64 __fastcall GetUploadFileSize(TTMSFNCCloudBaseRequest* const ARequest) = 0 ;
	virtual void __fastcall ExecuteRequest(TTMSFNCCloudBaseRequestResult* const ARequest) = 0 ;
	virtual void __fastcall RunExternalBrowser(const System::UnicodeString AURL, const System::UnicodeString ACallBackURL, const TTMSFNCCloudBaseExternalBrowserCallBackEvent ACallBack = 0x0) = 0 ;
	virtual void __fastcall CloseBrowser() = 0 ;
};

__interface  INTERFACE_UUID("{4876E6F4-B5D4-4322-A0B9-749040C239A5}") ITMSFNCCloudBaseService  : public System::IInterface 
{
	virtual _di_ITMSFNCCustomCloudBase __fastcall CreateCloudBase(TTMSFNCCustomCloudBase* const AValue) = 0 ;
	virtual void __fastcall DestroyCloudBase(const _di_ITMSFNCCustomCloudBase AValue) = 0 ;
};

class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestHeader : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FValue;
	
public:
	__fastcall TTMSFNCCloudBaseRequestHeader(const System::UnicodeString AName, const System::UnicodeString AValue);
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString Value = {read=FValue, write=FValue};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequestHeader() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestHeaders : public System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudBaseRequestHeader*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudBaseRequestHeader*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequestHeaders() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudBaseRequestHeader*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudBaseRequestHeader*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequestHeader>.Create */ inline __fastcall TTMSFNCCloudBaseRequestHeaders(TTMSFNCCloudBaseRequestHeader* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequestHeader*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequests : public System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudBaseRequest*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudBaseRequest*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequests() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudBaseRequest*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudBaseRequest*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudBaseRequest>.Create */ inline __fastcall TTMSFNCCloudBaseRequests(TTMSFNCCloudBaseRequest* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudBaseRequest*>(Values, Values_High) { }
	
};


enum DECLSPEC_DENUM TTMSFNCCloudBaseRequestMethod : unsigned char { rmGET, rmPOST, rmPOSTMULTIPART, rmPOSTMULTIPARTRELATED, rmPUT, rmPUTMULTIPART, rmPUTMULTIPARTRELATED, rmDELETE, rmPATCH, rmUPDATE };

enum DECLSPEC_DENUM TTMSFNCCloudBaseRequestScheme : unsigned char { rsHTTP, rsHTTPS };

enum DECLSPEC_DENUM TTMSFNCCloudBaseRequestResultType : unsigned char { rrtString, rrtStream, rrtFile };

enum DECLSPEC_DENUM TTMSFNCCloudBaseRequestCurlParserMode : unsigned char { cpmNone, cpmHeader, cpmFormData, cpmData, cpmDataBinary, cpmDataRaw, cpmDataUrlEncode, cpmUser, cpmBearer, cpmRequest, cpmResultFile };

class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestCurlParserFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FFileName;
	
public:
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCCloudBaseRequestCurlParserFile() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequestCurlParserFile() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestCurlParser : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCCloudBaseRequest* FRequest;
	
public:
	TTMSFNCCloudBaseRequest* __fastcall Parse(System::UnicodeString ACurl);
	__fastcall TTMSFNCCloudBaseRequestCurlParser();
	__fastcall virtual ~TTMSFNCCloudBaseRequestCurlParser();
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


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestPostDataBuilder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FPostData;
	
protected:
	void __fastcall AddInternalData(System::UnicodeString ADataType, System::UnicodeString AName, System::UnicodeString AValue, bool ASkipBoundary, System::UnicodeString AFileName, System::UnicodeString AContentType, System::UnicodeString AContentTransferEncoding, bool ASkipLineBreakOnEnd);
	
public:
	void __fastcall Clear();
	void __fastcall AddText(System::UnicodeString AText, bool ALineBreak = true);
	void __fastcall AddContentType(System::UnicodeString AValue);
	void __fastcall AddLineBreak();
	void __fastcall AddAttachment(System::UnicodeString AName, System::UnicodeString AValue, bool ASkipBoundary = false, System::UnicodeString AFileName = System::UnicodeString(), System::UnicodeString AContentType = System::UnicodeString(), System::UnicodeString AContentTransferEncoding = System::UnicodeString(), bool ASkipLineBreakOnEnd = false);
	void __fastcall AddHeadBoundary();
	void __fastcall AddTailBoundary();
	void __fastcall AddFormData(System::UnicodeString AName, System::UnicodeString AValue, bool ASkipBoundary = false, System::UnicodeString AFileName = System::UnicodeString(), System::UnicodeString AContentType = System::UnicodeString(), System::UnicodeString AContentTransferEncoding = System::UnicodeString(), bool ASkipLineBreakOnEnd = false);
	void __fastcall Close();
	System::UnicodeString __fastcall Build();
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCCloudBaseRequestPostDataBuilder() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseRequestPostDataBuilder() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCCloudBaseRequestHeaders* FHeaders;
	System::UnicodeString FAgent;
	System::UnicodeString FHost;
	int FPort;
	System::UnicodeString FPath;
	System::UnicodeString FQuery;
	TTMSFNCCloudBaseRequestMethod FMethod;
	System::UnicodeString FName;
	System::UnicodeString FPostData;
	void *FDataPointer;
	bool FDataBoolean;
	System::UnicodeString FDataString;
	System::TObject* FDataObject;
	NativeInt FDataInteger;
	TTMSFNCCloudBaseRequestResultType FResultType;
	System::Classes::TMemoryStream* FUploadStream;
	System::UnicodeString FUploadFile;
	System::UnicodeString FResultFile;
	bool FCustomHeaders;
	System::UnicodeString FDataUpload;
	__int64 FResultFileSize;
	__int64 FDataInt64;
	System::Classes::TMemoryStream* __fastcall GetUploadStream();
	TTMSFNCCloudBaseRequestHeaders* __fastcall GetHeaders();
	
protected:
	TTMSFNCCloudBaseRequestHeader* __fastcall DuplicateHeader(TTMSFNCCloudBaseRequestHeader* AHeader);
	
public:
	__fastcall virtual TTMSFNCCloudBaseRequest();
	__fastcall virtual ~TTMSFNCCloudBaseRequest();
	void __fastcall Assign(TTMSFNCCloudBaseRequest* ARequest);
	virtual void __fastcall Clear();
	virtual void __fastcall ClearHeaders();
	bool __fastcall HasUploadStream();
	bool __fastcall HasUploadFile();
	System::UnicodeString __fastcall GenerateHeaders();
	System::UnicodeString __fastcall GetMethodString();
	System::UnicodeString __fastcall GetURL();
	System::UnicodeString __fastcall GetServer();
	System::UnicodeString __fastcall GetQueryPath();
	int __fastcall GetPort();
	TTMSFNCCloudBaseRequestHeader* __fastcall AddHeader(const System::UnicodeString AName, const System::UnicodeString AValue);
	TTMSFNCCloudBaseRequestHeader* __fastcall AddBasicAuthHeader(const System::UnicodeString AUserName, const System::UnicodeString APassword);
	__property bool CustomHeaders = {read=FCustomHeaders, write=FCustomHeaders, nodefault};
	__property System::UnicodeString DataUpload = {read=FDataUpload, write=FDataUpload};
	__property void * DataPointer = {read=FDataPointer, write=FDataPointer};
	__property bool DataBoolean = {read=FDataBoolean, write=FDataBoolean, nodefault};
	__property System::TObject* DataObject = {read=FDataObject, write=FDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=FDataString};
	__property NativeInt DataInteger = {read=FDataInteger, write=FDataInteger};
	__property __int64 DataInt64 = {read=FDataInt64, write=FDataInt64};
	__property TTMSFNCCloudBaseRequestHeaders* Headers = {read=GetHeaders};
	__property System::UnicodeString Agent = {read=FAgent, write=FAgent};
	__property System::UnicodeString PostData = {read=FPostData, write=FPostData};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::UnicodeString Path = {read=FPath, write=FPath};
	__property System::UnicodeString Query = {read=FQuery, write=FQuery};
	__property TTMSFNCCloudBaseRequestMethod Method = {read=FMethod, write=FMethod, default=0};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property TTMSFNCCloudBaseRequestResultType ResultType = {read=FResultType, write=FResultType, default=0};
	__property System::UnicodeString ResultFile = {read=FResultFile, write=FResultFile};
	__property __int64 ResultFileSize = {read=FResultFileSize, write=FResultFileSize};
	__property System::Classes::TMemoryStream* UploadStream = {read=GetUploadStream};
	__property System::UnicodeString UploadFile = {read=FUploadFile, write=FUploadFile};
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


class PASCALIMPLEMENTATION TTMSFNCCloudBaseRequestResult : public TTMSFNCCloudBaseRequest
{
	typedef TTMSFNCCloudBaseRequest inherited;
	
private:
	TTMSFNCCloudBaseRequestHeaders* FResponseHeaders;
	System::UnicodeString FMessage;
	bool FIsDestroying;
	System::UnicodeString FResultString;
	_di_TTMSFNCCloudBaseRequestResultEvent FCallBackResult;
	_di_TTMSFNCCloudBaseRequestResultProgressEvent FCallBackProgress;
	bool FAsync;
	System::Threading::_di_ITask FTask;
	int FResponseCode;
	float FProgress;
	bool FRunning;
	TTMSFNCCloudBaseRequestEvent FOnRemove;
	TTMSFNCCloudBaseRequestEvent FOnComplete;
	TTMSFNCCloudBaseRequestLogEvent FOnLog;
	TTMSFNCCloudBaseRequestProgressEvent FOnProgress;
	System::Classes::TMemoryStream* FResultStream;
	bool FSuccess;
	TTMSFNCCloudBaseRequestEvent FOnCancelled;
	
protected:
	__int64 FTotalBytes;
	__int64 FBytesReceived;
	bool FDone;
	void __fastcall ProcessResult();
	void __fastcall ProcessProgress();
	void __fastcall RequestResult();
	void __fastcall ProcessLog();
	void __fastcall RequestProgress(float AProgress, bool AUpload);
	bool __fastcall IsAsyncAvailable();
	__property TTMSFNCCloudBaseRequestEvent OnRemove = {read=FOnRemove, write=FOnRemove};
	__property float Progress = {read=FProgress};
	
public:
	__fastcall TTMSFNCCloudBaseRequestResult()/* overload */;
	__fastcall TTMSFNCCloudBaseRequestResult(TTMSFNCCloudBaseRequest* const ARequest, const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent, const _di_TTMSFNCCloudBaseRequestResultProgressEvent ARequestResultProgressEvent, const bool AAsync, const System::Threading::_di_ITask ATask)/* overload */;
	__fastcall virtual ~TTMSFNCCloudBaseRequestResult();
	virtual void __fastcall Clear();
	virtual void __fastcall Cancel();
	HIDESBASE virtual void __fastcall Log(System::UnicodeString AMessage);
	bool __fastcall CheckTaskStatus();
	System::UnicodeString __fastcall GetResponseHeader(System::UnicodeString AName);
	__property __int64 ResultBytesReceived = {read=FBytesReceived};
	__property __int64 ResultTotalBytes = {read=FTotalBytes};
	__property System::UnicodeString ResultString = {read=FResultString, write=FResultString};
	__property System::Classes::TMemoryStream* ResultStream = {read=FResultStream, write=FResultStream};
	__property int ResponseCode = {read=FResponseCode, write=FResponseCode, nodefault};
	__property TTMSFNCCloudBaseRequestHeaders* ResponseHeaders = {read=FResponseHeaders};
	__property _di_TTMSFNCCloudBaseRequestResultEvent CallBackResult = {read=FCallBackResult, write=FCallBackResult};
	__property _di_TTMSFNCCloudBaseRequestResultProgressEvent CallBackProgress = {read=FCallBackProgress, write=FCallBackProgress};
	__property bool Async = {read=FAsync, write=FAsync, nodefault};
	__property bool Success = {read=FSuccess, write=FSuccess, nodefault};
	__property System::Threading::_di_ITask Task = {read=FTask, write=FTask};
	__property bool Running = {read=FRunning, write=FRunning, default=0};
	__property TTMSFNCCloudBaseRequestLogEvent OnLog = {read=FOnLog, write=FOnLog};
	__property TTMSFNCCloudBaseRequestProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TTMSFNCCloudBaseRequestEvent OnComplete = {read=FOnComplete, write=FOnComplete};
	__property TTMSFNCCloudBaseRequestEvent OnCancelled = {read=FOnCancelled, write=FOnCancelled};
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseService : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDeveloperURL;
	System::UnicodeString FName;
	System::UnicodeString FBaseURL;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString DeveloperURL = {read=FDeveloperURL, write=FDeveloperURL};
	__property System::UnicodeString BaseURL = {read=FBaseURL, write=FBaseURL};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseService() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCCloudBaseService() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomCloudBase : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	TTMSFNCCloudBaseRequestPostDataBuilder* FPostDataBuilder;
	_di_ITMSFNCCustomCloudBase FCloudBase;
	TTMSFNCCloudBaseRequest* FRequest;
	TTMSFNCCloudBaseRequestResult* FRequestResult;
	TTMSFNCCloudBaseRequests* FRequests;
	TTMSFNCCloudBaseLogEvent FOnRequestLog;
	bool FLogging;
	System::UnicodeString FLogFileName;
	TTMSFNCCloudBaseCompleteEvent FOnRequestComplete;
	TTMSFNCCloudBaseProgressEvent FOnRequestProgress;
	TTMSFNCCloudBaseService* FService;
	System::Classes::TStrings* FScopes;
	System::Classes::TNotifyEvent FOnRequestsComplete;
	TTMSFNCCloudBaseStartedEvent FOnRequestStarted;
	TTMSFNCCloudBaseCancelledEvent FOnRequestCancelled;
	bool __fastcall IsLogFileNameStored();
	void __fastcall SetService(TTMSFNCCloudBaseService* const Value);
	void __fastcall SetScopes(System::Classes::TStrings* const Value);
	TTMSFNCCloudBaseRequestPostDataBuilder* __fastcall GetPostDataBuilder();
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetVersion();
	System::UnicodeString __fastcall GetScopes(System::WideChar ADelimiter, bool AEncode = false);
	__classmethod TTMSFNCCustomCloudBase* __fastcall GetCloudBaseGlobal();
	__classmethod TTMSFNCCloudBaseRequestPostDataBuilder* __fastcall GetPostDataBuilderGlobal();
	__classmethod TTMSFNCCloudBaseRequestHeaders* __fastcall GetHeadersGlobal();
	HIDESBASE virtual void __fastcall Log(TTMSFNCCloudBaseRequestResult* const ARequestResult, System::UnicodeString AMessage);
	__classmethod void __fastcall DoCurlRequestComplete(TTMSFNCCloudBaseRequestResult* const ARequestResult);
	void __fastcall DoRequestRemove(TTMSFNCCloudBaseRequestResult* const ARequestResult);
	void __fastcall DoRequestComplete(TTMSFNCCloudBaseRequestResult* const ARequestResult);
	void __fastcall DoRequestCancelled(TTMSFNCCloudBaseRequestResult* const ARequestResult);
	void __fastcall DoRequestStarted(TTMSFNCCloudBaseRequestResult* const ARequestResult);
	void __fastcall DoRequestsComplete();
	void __fastcall DoRequestProgress(TTMSFNCCloudBaseRequestResult* const ARequestResult, float AProgress, bool AUpload);
	void __fastcall DoRequestLog(TTMSFNCCloudBaseRequestResult* const ARequestResult, System::UnicodeString ALogMessage);
	void __fastcall InternalExecuteRequest(TTMSFNCCloudBaseRequestResult* const ARequest);
	__classmethod void __fastcall InternalCurl(const System::UnicodeString ACurl, _di_TTMSFNCCloudBaseRequestResultEvent ACallbackRequest, _di_TTMSFNCCloudBaseRequestResultStringEvent ACallbackString, _di_TTMSFNCCloudBaseRequestResultStreamEvent ACallbackStream)/* overload */;
	__classmethod void __fastcall InternalSimpleGETAsync(const System::UnicodeString AURL, const TTMSFNCCloudBaseRequestResultType AResultType, const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent);
	__classmethod TTMSFNCCloudBaseRequestResult* __fastcall InternalSimpleGETSync(const System::UnicodeString AURL, const TTMSFNCCloudBaseRequestResultType AResultType);
	__property _di_ITMSFNCCustomCloudBase CloudBase = {read=FCloudBase};
	__property System::UnicodeString Version = {read=GetVersion};
	__property TTMSFNCCloudBaseService* Service = {read=FService, write=SetService};
	__property TTMSFNCCloudBaseLogEvent OnRequestLog = {read=FOnRequestLog, write=FOnRequestLog};
	__property TTMSFNCCloudBaseProgressEvent OnRequestProgress = {read=FOnRequestProgress, write=FOnRequestProgress};
	__property TTMSFNCCloudBaseCompleteEvent OnRequestComplete = {read=FOnRequestComplete, write=FOnRequestComplete};
	__property TTMSFNCCloudBaseCancelledEvent OnRequestCancelled = {read=FOnRequestCancelled, write=FOnRequestCancelled};
	__property TTMSFNCCloudBaseStartedEvent OnRequestStarted = {read=FOnRequestStarted, write=FOnRequestStarted};
	__property System::Classes::TNotifyEvent OnRequestsComplete = {read=FOnRequestsComplete, write=FOnRequestsComplete};
	__property bool Logging = {read=FLogging, write=FLogging, default=0};
	__property System::UnicodeString LogFileName = {read=FLogFileName, write=FLogFileName, stored=IsLogFileNameStored};
	
public:
	__fastcall virtual TTMSFNCCustomCloudBase()/* overload */;
	__fastcall virtual TTMSFNCCustomCloudBase(System::Classes::TComponent* AOwner)/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCCustomCloudBase();
	void __fastcall ExecuteAllRequests();
	virtual void __fastcall Test();
	virtual void __fastcall CancelRequests();
	virtual void __fastcall CancelRequest(TTMSFNCCloudBaseRequestResult* const ARequest);
	virtual __int64 __fastcall GetUploadFileSize(TTMSFNCCloudBaseRequest* const ARequest);
	virtual int __fastcall GetRequestCount(const bool IsRunning = false, const System::UnicodeString ARequestName = System::UnicodeString());
	virtual TTMSFNCCloudBaseRequestResult* __fastcall AddRequest(const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent = _di_TTMSFNCCloudBaseRequestResultEvent(), const _di_TTMSFNCCloudBaseRequestResultProgressEvent ARequestResultProgressEvent = _di_TTMSFNCCloudBaseRequestResultProgressEvent(), const bool AAsync = true);
	virtual TTMSFNCCloudBaseRequestResult* __fastcall ExecuteRequest(const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent = _di_TTMSFNCCloudBaseRequestResultEvent(), const _di_TTMSFNCCloudBaseRequestResultProgressEvent ARequestResultProgressEvent = _di_TTMSFNCCloudBaseRequestResultProgressEvent(), const bool AAsync = true);
	__property TTMSFNCCloudBaseRequest* Request = {read=FRequest};
	__property TTMSFNCCloudBaseRequestResult* RequestResult = {read=FRequestResult};
	__property TTMSFNCCloudBaseRequestPostDataBuilder* PostDataBuilder = {read=GetPostDataBuilder};
	__property System::Classes::TStrings* Scopes = {read=FScopes, write=SetScopes};
	__property TTMSFNCCloudBaseRequests* RunningRequests = {read=FRequests};
	__classmethod void __fastcall DownloadFileFromURL(const System::UnicodeString AURL, const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent);
	__classmethod void __fastcall SimpleGETAsyncAsString(const System::UnicodeString AURL, const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent);
	__classmethod void __fastcall SimpleGETAsyncAsStream(const System::UnicodeString AURL, const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent);
	__classmethod System::UnicodeString __fastcall SimpleGETSyncAsString(const System::UnicodeString AURL);
	__classmethod System::Classes::TMemoryStream* __fastcall SimpleGETSyncAsStream(const System::UnicodeString AURL);
	__classmethod void __fastcall Curl(const System::UnicodeString ACurl, _di_TTMSFNCCloudBaseRequestResultEvent ACallback)/* overload */;
	__classmethod void __fastcall Curl(const System::UnicodeString ACurl, _di_TTMSFNCCloudBaseRequestResultStringEvent ACallback)/* overload */;
	__classmethod void __fastcall Curl(const System::UnicodeString ACurl, _di_TTMSFNCCloudBaseRequestResultStreamEvent ACallback)/* overload */;
};


class PASCALIMPLEMENTATION TTMSFNCCloudBase : public TTMSFNCCustomCloudBase
{
	typedef TTMSFNCCustomCloudBase inherited;
	
__published:
	__property Logging = {default=0};
	__property LogFileName;
	__property Version = {default=0};
	__property OnRequestsComplete;
	__property OnRequestLog;
	__property OnRequestComplete;
	__property OnRequestCancelled;
	__property OnRequestStarted;
	__property OnRequestProgress;
public:
	/* TTMSFNCCustomCloudBase.Create */ inline __fastcall virtual TTMSFNCCloudBase()/* overload */ : TTMSFNCCustomCloudBase() { }
	/* TTMSFNCCustomCloudBase.Create */ inline __fastcall virtual TTMSFNCCloudBase(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomCloudBase(AOwner) { }
	/* TTMSFNCCustomCloudBase.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBase() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseList : public System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase>
{
	typedef System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_ITMSFNCCustomCloudBase>.Create */ inline __fastcall TTMSFNCCloudBaseList()/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_ITMSFNCCustomCloudBase>.Create */ inline __fastcall TTMSFNCCloudBaseList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<_di_ITMSFNCCustomCloudBase> > AComparer)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_ITMSFNCCustomCloudBase>.Create */ inline __fastcall TTMSFNCCloudBaseList(System::Generics::Collections::TEnumerable__1<_di_ITMSFNCCustomCloudBase>* const Collection)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_ITMSFNCCustomCloudBase>.Create */ inline __fastcall TTMSFNCCloudBaseList(const _di_ITMSFNCCustomCloudBase *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<_di_ITMSFNCCustomCloudBase>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_ITMSFNCCustomCloudBase>.Destroy */ inline __fastcall virtual ~TTMSFNCCloudBaseList() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBaseFactoryService : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	TTMSFNCCloudBaseList* FCloudBases;
	virtual _di_ITMSFNCCustomCloudBase __fastcall DoCreateCloudBase(TTMSFNCCustomCloudBase* const AValue) = 0 ;
	void __fastcall DoRemoveCloudBase(const _di_ITMSFNCCustomCloudBase AValue);
	
public:
	__fastcall TTMSFNCCloudBaseFactoryService();
	__fastcall virtual ~TTMSFNCCloudBaseFactoryService();
	_di_ITMSFNCCustomCloudBase __fastcall CreateCloudBase(TTMSFNCCustomCloudBase* const AValue);
	void __fastcall DestroyCloudBase(const _di_ITMSFNCCustomCloudBase AValue);
private:
	void *__ITMSFNCCloudBaseService;	// ITMSFNCCloudBaseService 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {4876E6F4-B5D4-4322-A0B9-749040C239A5}
	operator _di_ITMSFNCCloudBaseService()
	{
		_di_ITMSFNCCloudBaseService intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCCloudBaseService*(void) { return (ITMSFNCCloudBaseService*)&__ITMSFNCCloudBaseService; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudPlatformServicesService : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::_di_IInterface FInterface;
	System::UnicodeString FGUID;
	
public:
	__fastcall TTMSFNCCloudPlatformServicesService(System::UnicodeString AGUID, System::_di_IInterface AInterface);
	__property System::UnicodeString GUID = {read=FGUID};
	__property System::_di_IInterface Interface = {read=FInterface};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCCloudPlatformServicesService() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudPlatformServicesList : public System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*> inherited;
	
private:
	System::_di_IInterface __fastcall GetValue(System::UnicodeString AGUID);
	
public:
	bool __fastcall ContainsKey(System::UnicodeString AGUID);
	void __fastcall RemoveByGUID(System::UnicodeString AGUID);
	__property System::_di_IInterface Interfaces[System::UnicodeString AGUID] = {read=GetValue};
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudPlatformServicesService*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudPlatformServicesService*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Destroy */ inline __fastcall virtual ~TTMSFNCCloudPlatformServicesList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCCloudPlatformServicesService*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCCloudPlatformServicesService*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCCloudBase_TTMSFNCCloudPlatformServicesService>.Create */ inline __fastcall TTMSFNCCloudPlatformServicesList(TTMSFNCCloudPlatformServicesService* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCCloudPlatformServicesService*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCloudBasePlatformServices : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCCloudPlatformServicesList* FServicesList;
	static TTMSFNCCloudBasePlatformServices* FCurrent;
	static bool FCurrentReleased;
	__classmethod void __fastcall ReleaseCurrent();
	
public:
	__fastcall TTMSFNCCloudBasePlatformServices();
	__fastcall virtual ~TTMSFNCCloudBasePlatformServices();
	void __fastcall AddPlatformService(const GUID &AServiceGUID, const System::_di_IInterface AService);
	void __fastcall RemovePlatformService(const GUID &AServiceGUID);
	System::_di_IInterface __fastcall GetPlatformService(const GUID &AServiceGUID);
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID)/* overload */;
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID, System::_di_IInterface &AService)/* overload */;
	__classmethod TTMSFNCCloudBasePlatformServices* __fastcall Current();
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


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x1);
static constexpr System::Int8 BLD_VER = System::Int8(0x1);
#define sRequestLineBreak u"\r\n"
#define sRequestBoundary u"AaB03x"
#define sRequestHeadBoundary u"--AaB03x"
#define sRequestTailBoundary u"--AaB03x--"
#define ValidHTTPCodes (System::Set<System::Byte, 0, 255>() << 0xc8 << 0xc9 << 0xca << 0xcc )
extern DELPHI_PACKAGE TTMSFNCCloudBaseRequestPostDataBuilder* __fastcall HTTPPostDataBuilder(void);
extern DELPHI_PACKAGE void __fastcall HTTPClearHeaders(void);
extern DELPHI_PACKAGE void __fastcall HTTPAddHeader(const System::UnicodeString AName, const System::UnicodeString AValue);
extern DELPHI_PACKAGE void __fastcall HTTPCloudRequest(const System::UnicodeString AURL, TTMSFNCCloudBaseRequestResultType AResultType = (TTMSFNCCloudBaseRequestResultType)(0x0), TTMSFNCCloudBaseRequestMethod AMethod = (TTMSFNCCloudBaseRequestMethod)(0x0), const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent = _di_TTMSFNCCloudBaseRequestResultEvent())/* overload */;
extern DELPHI_PACKAGE void __fastcall HTTPCloudRequest(const System::UnicodeString AHost, const System::UnicodeString APath, const System::UnicodeString AQuery, const System::UnicodeString APostData, TTMSFNCCloudBaseRequestResultType AResultType = (TTMSFNCCloudBaseRequestResultType)(0x0), TTMSFNCCloudBaseRequestMethod AMethod = (TTMSFNCCloudBaseRequestMethod)(0x1), const _di_TTMSFNCCloudBaseRequestResultEvent ARequestResultEvent = _di_TTMSFNCCloudBaseRequestResultEvent())/* overload */;
}	/* namespace Tmsfnccloudbase */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCLOUDBASE)
using namespace Fmx::Tmsfnccloudbase;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccloudbaseHPP
