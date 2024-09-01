// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Coinbase.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_coinbaseHPP
#define Sgcwebsocket_api_coinbaseHPP

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
#include <sgcWebSocket_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcHTTP_API_Coinbase.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_coinbase
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSCoinbaseLog_Options;
class DELPHICLASS TsgcWSCoinbase_Options;
class DELPHICLASS TsgcWS_API_Coinbase;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSCoinbaseSubscriptionsEvent)(System::TObject* Sender, System::UnicodeString aChannels, System::UnicodeString aProductIds, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSCoinbaseMessageEvent)(System::TObject* Sender, System::UnicodeString aType, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSCoinbaseErrorEvent)(System::TObject* Sender, System::UnicodeString aError, System::UnicodeString aReason, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSCoinbaseHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TsgcWSCoinbaseRequest : unsigned char { cbrSubscribe, cbrUnsubscribe };

enum DECLSPEC_DENUM TsgcWSCoinbaseChannels : unsigned char { cbcHeartBeat, cbcStatus, cbcTicker, cbcLevel2, cbcMatches, cbcFull, cbcUser };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSCoinbaseLog_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FFileName;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSCoinbaseLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSCoinbaseLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSCoinbase_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcWSCoinbaseLog_Options* FHTTPLogOptions;
	System::UnicodeString FPassphrase;
	bool FSandBox;
	void __fastcall SetHTTPLogOptions(TsgcWSCoinbaseLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSCoinbase_Options();
	__fastcall virtual ~TsgcWSCoinbase_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcWSCoinbaseLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
	__property System::UnicodeString Passphrase = {read=FPassphrase, write=FPassphrase};
	__property bool SandBox = {read=FSandBox, write=FSandBox, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Coinbase : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	System::UnicodeString __fastcall GetJSONChannels();
	System::UnicodeString __fastcall GetJSONProductIds();
	System::UnicodeString __fastcall GetJSONError();
	System::UnicodeString __fastcall GetJSONErrorReason();
	virtual void __fastcall DoReadJSONType(System::UnicodeString aRawMessage);
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSCoinbase_Options* FCoinbase;
	Sgchttp_api_coinbase::TsgcHTTP_API_Coinbase_Rest* FREST_API;
	void __fastcall SetCoinbase(TsgcWSCoinbase_Options* const Value);
	Sgchttp_api_coinbase::TsgcHTTP_API_Coinbase_Rest* __fastcall GetREST_API();
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSCoinbase_Options* Coinbase = {read=FCoinbase, write=SetCoinbase};
	__property Sgchttp_api_coinbase::TsgcHTTP_API_Coinbase_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	__fastcall virtual TsgcWS_API_Coinbase(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Coinbase();
	
private:
	System::UnicodeString __fastcall GetChannel(TsgcWSCoinbaseChannels aChannel);
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoRequest(TsgcWSCoinbaseRequest aRequest, TsgcWSCoinbaseChannels aChannel, const System::UnicodeString aProductId = System::UnicodeString(), bool aAuthenticate = false);
	
public:
	void __fastcall SubscribeHeartBeat(const System::UnicodeString aProductId, bool aAuthenticate = false);
	void __fastcall UnSubscribeHeartBeat(const System::UnicodeString aProductId);
	void __fastcall SubscribeStatus(bool aAuthenticate = false);
	void __fastcall UnSubscribeStatus();
	void __fastcall SubscribeTicker(const System::UnicodeString aProductId, bool aAuthenticate = false);
	void __fastcall UnSubscribeTicker(const System::UnicodeString aProductId);
	void __fastcall SubscribeLevel2(const System::UnicodeString aProductId, bool aAuthenticate = false);
	void __fastcall UnSubscribeLevel2(const System::UnicodeString aProductId);
	void __fastcall SubscribeMatches(const System::UnicodeString aProductId, bool aAuthenticate = false);
	void __fastcall UnSubscribeMatches(const System::UnicodeString aProductId);
	void __fastcall SubscribeFull(const System::UnicodeString aProductId, bool aAuthenticate = false);
	void __fastcall UnSubscribeFull(const System::UnicodeString aProductId);
	void __fastcall SubscribeUser(const System::UnicodeString aProductId);
	void __fastcall UnSubscribeUser(const System::UnicodeString aProductId);
	
private:
	TsgcWSCoinbaseErrorEvent FOnCoinbaseError;
	TsgcWSCoinbaseMessageEvent FOnCoinbaseMessage;
	TsgcWSCoinbaseSubscriptionsEvent FOnCoinbaseSubscriptions;
	
protected:
	virtual void __fastcall DoSubscriptionsEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoMessageEvent(const System::UnicodeString aType, const System::UnicodeString aRawMessage);
	virtual void __fastcall DoErrorEvent(const System::UnicodeString aRawMessage);
	
public:
	__property TsgcWSCoinbaseErrorEvent OnCoinbaseError = {read=FOnCoinbaseError, write=FOnCoinbaseError};
	__property TsgcWSCoinbaseMessageEvent OnCoinbaseMessage = {read=FOnCoinbaseMessage, write=FOnCoinbaseMessage};
	__property TsgcWSCoinbaseSubscriptionsEvent OnCoinbaseSubscriptions = {read=FOnCoinbaseSubscriptions, write=FOnCoinbaseSubscriptions};
	
private:
	TsgcWSCoinbaseHTTPExceptionEvent FOnCoinbaseHTTPException;
	
protected:
	virtual void __fastcall DoCoinbaseHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSCoinbaseHTTPExceptionEvent OnCoinbaseHTTPException = {read=FOnCoinbaseHTTPException, write=FOnCoinbaseHTTPException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_coinbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_COINBASE)
using namespace Sgcwebsocket_api_coinbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_coinbaseHPP
