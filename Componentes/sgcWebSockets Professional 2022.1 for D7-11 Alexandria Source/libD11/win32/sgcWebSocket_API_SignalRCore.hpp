// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_SignalRCore.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_signalrcoreHPP
#define Sgcwebsocket_api_signalrcoreHPP

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
#include <sgcWebSocket_Types.hpp>
#include <sgcJSON.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_signalrcore
{
//-- forward type declarations -----------------------------------------------
struct TSignalRCore_Connect;
struct TSignalRCore_Invocation;
struct TSignalRCore_StreamItem;
struct TSignalRCore_Completion;
struct TSignalRCore_CancelInvocation;
struct TSignalRCore_Error;
struct TSignalRCore_Close;
struct TSignalRCore_WaitForCompletion;
class DELPHICLASS TThreadInvoke;
class DELPHICLASS TsgcWSSignalRCore_Authentication_RequestToken;
class DELPHICLASS TsgcWSSignalRCore_Authentication_SetToken;
class DELPHICLASS TsgcWSSignalRCore_Authentication;
class DELPHICLASS TsgcWSSignalRCore_Options;
class DELPHICLASS TsgcWS_API_SignalRCore;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSSignalRCoreAuthentication : unsigned char { srcaRequestToken, srcaSetToken };

struct DECLSPEC_DRECORD TSignalRCore_Connect
{
public:
	System::UnicodeString ConnectionId;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_Invocation
{
public:
	System::UnicodeString Target;
	System::UnicodeString Arguments;
	System::UnicodeString InvocationId;
	System::UnicodeString Headers;
	System::UnicodeString _RawMessage;
};


typedef TSignalRCore_Invocation TSignalRCore_StreamInvocation;

struct DECLSPEC_DRECORD TSignalRCore_StreamItem
{
public:
	System::UnicodeString InvocationId;
	System::UnicodeString Item;
	System::UnicodeString Headers;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_Completion
{
public:
	System::UnicodeString InvocationId;
	System::UnicodeString Result;
	System::UnicodeString Error;
	System::UnicodeString Headers;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_CancelInvocation
{
public:
	System::UnicodeString InvocationId;
	System::UnicodeString Headers;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_Error
{
public:
	System::UnicodeString Error;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_Close
{
public:
	System::UnicodeString Error;
	System::UnicodeString _RawMessage;
};


struct DECLSPEC_DRECORD TSignalRCore_WaitForCompletion
{
public:
	System::UnicodeString InvocationId;
	bool Waiting;
	TSignalRCore_Completion Completion;
};


typedef void __fastcall (__closure *TsgcWSSignalRCoreBeforeConnectEvent)(System::TObject* Sender, System::UnicodeString &ConnectionId);

typedef void __fastcall (__closure *TsgcWSSignalRCoreConnectEvent)(System::TObject* Sender, const TSignalRCore_Connect &Connect);

typedef void __fastcall (__closure *TsgcWSSignalRCoreInvocationEvent)(System::TObject* Sender, const TSignalRCore_Invocation &Invocation);

typedef void __fastcall (__closure *TsgcWSSignalRCoreStreamInvocationEvent)(System::TObject* Sender, const TSignalRCore_Invocation &StreamInvocation);

typedef void __fastcall (__closure *TsgcWSSignalRCoreStreamItemEvent)(System::TObject* Sender, const TSignalRCore_StreamItem &StreamItem, bool &Cancel);

typedef void __fastcall (__closure *TsgcWSSignalRCoreCompletionEvent)(System::TObject* Sender, const TSignalRCore_Completion &Completion);

typedef void __fastcall (__closure *TsgcWSSignalRCoreCancelInvocationEvent)(System::TObject* Sender, const TSignalRCore_CancelInvocation &CancelInvocation);

typedef void __fastcall (__closure *TsgcWSSignalRCoreKeepAliveEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSSignalRCoreErrorEvent)(System::TObject* Sender, const TSignalRCore_Error &Error);

typedef void __fastcall (__closure *TsgcWSSignalRCoreCloseEvent)(System::TObject* Sender, const TSignalRCore_Close &Close);

typedef void __fastcall (__closure *TsgcWSSignalRCoreDisconnectEvent)(System::TObject* Sender, int CloseCode, System::UnicodeString CloseReason);

class PASCALIMPLEMENTATION TThreadInvoke : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	System::UnicodeString FArguments;
	TSignalRCore_Completion FCompletion;
	System::UnicodeString FInvocationId;
	TsgcWS_API_SignalRCore* FSignalRCore;
	System::UnicodeString FTarget;
	int FTimeout;
	Sgcwebsocket_types::TwsSignalRCoreMessages FMessage;
	
protected:
	virtual void __fastcall Execute();
	
public:
	void __fastcall Invoke(TsgcWS_API_SignalRCore* aSignalRCore, System::UnicodeString aTarget, System::UnicodeString aArguments, System::UnicodeString aInvocationId, int aTimeout = 0x2710);
	void __fastcall InvokeStream(TsgcWS_API_SignalRCore* aSignalRCore, System::UnicodeString aTarget, System::UnicodeString aArguments, System::UnicodeString aInvocationId, int aTimeout = 0x2710);
	__property TSignalRCore_Completion Completion = {read=FCompletion, write=FCompletion};
public:
	/* TThread.Create */ inline __fastcall TThreadInvoke()/* overload */ : System::Classes::TThread() { }
	/* TThread.Create */ inline __fastcall TThreadInvoke(bool CreateSuspended)/* overload */ : System::Classes::TThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TThreadInvoke(bool CreateSuspended, NativeUInt ReservedStackSize)/* overload */ : System::Classes::TThread(CreateSuspended, ReservedStackSize) { }
	/* TThread.Destroy */ inline __fastcall virtual ~TThreadInvoke() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalRCore_Authentication_RequestToken : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FPostFieldPassword;
	System::UnicodeString FPostFieldToken;
	System::UnicodeString FURL;
	System::UnicodeString FPostFieldUsername;
	System::UnicodeString FQueryFieldToken;
	
public:
	__fastcall virtual TsgcWSSignalRCore_Authentication_RequestToken();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString PostFieldPassword = {read=FPostFieldPassword, write=FPostFieldPassword};
	__property System::UnicodeString PostFieldToken = {read=FPostFieldToken, write=FPostFieldToken};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
	__property System::UnicodeString PostFieldUsername = {read=FPostFieldUsername, write=FPostFieldUsername};
	__property System::UnicodeString QueryFieldToken = {read=FQueryFieldToken, write=FQueryFieldToken};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSignalRCore_Authentication_RequestToken() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalRCore_Authentication_SetToken : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FToken;
	
public:
	__fastcall virtual TsgcWSSignalRCore_Authentication_SetToken();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Token = {read=FToken, write=FToken};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSSignalRCore_Authentication_SetToken() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalRCore_Authentication : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSSignalRCoreAuthentication FAuthentication;
	TsgcWSSignalRCore_Authentication_RequestToken* FRequestToken;
	bool FEnabled;
	System::UnicodeString FPassword;
	TsgcWSSignalRCore_Authentication_SetToken* FSetToken;
	System::UnicodeString FUsername;
	void __fastcall SetRequestToken(TsgcWSSignalRCore_Authentication_RequestToken* const Value);
	void __fastcall SetSetToken(TsgcWSSignalRCore_Authentication_SetToken* const Value);
	
public:
	__fastcall virtual TsgcWSSignalRCore_Authentication();
	__fastcall virtual ~TsgcWSSignalRCore_Authentication();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSSignalRCoreAuthentication Authentication = {read=FAuthentication, write=FAuthentication, nodefault};
	__property TsgcWSSignalRCore_Authentication_RequestToken* RequestToken = {read=FRequestToken, write=SetRequestToken};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property TsgcWSSignalRCore_Authentication_SetToken* SetToken = {read=FSetToken, write=SetSetToken};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSignalRCore_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSSignalRCore_Authentication* FAuthentication;
	System::UnicodeString FHub;
	Sgcwebsocket_types::TwsSignalRCoreProtocols FProtocol;
	Sgcwebsocket_types::TwsSignalRCoreProtocolVersion FVersion;
	void __fastcall SetAuthentication(TsgcWSSignalRCore_Authentication* const Value);
	
protected:
	System::UnicodeString __fastcall GetHubString();
	System::UnicodeString __fastcall GetProtocolString();
	int __fastcall GetVersionInteger();
	
public:
	__fastcall virtual TsgcWSSignalRCore_Options();
	__fastcall virtual ~TsgcWSSignalRCore_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSSignalRCore_Authentication* Authentication = {read=FAuthentication, write=SetAuthentication};
	__property System::UnicodeString Hub = {read=FHub, write=FHub};
	__property Sgcwebsocket_types::TwsSignalRCoreProtocols Protocol = {read=FProtocol, write=FProtocol, nodefault};
	__property Sgcwebsocket_types::TwsSignalRCoreProtocolVersion Version = {read=FVersion, write=FVersion, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_SignalRCore : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyError(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyException(Sgcwebsocket_classes::TsgcWSConnection* aConnection)/* overload */;
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoBeforeConnect();
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	void __fastcall SetSignalRCore(TsgcWSSignalRCore_Options* const Value);
	
protected:
	System::Variant __fastcall GetJSONValue(const System::UnicodeString aNode);
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSSignalRCore_Options* FSignalRCore;
	
public:
	__property TsgcWSSignalRCore_Options* SignalRCore = {read=FSignalRCore, write=SetSignalRCore};
	__fastcall virtual TsgcWS_API_SignalRCore(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_SignalRCore();
	
private:
	bool FFirstMessage;
	bool FNegotiate;
	System::UnicodeString FConnectionId;
	System::UnicodeString FAuthBearerToken;
	TThreadInvoke* FThreadInvoke;
	System::UnicodeString __fastcall GetArgumentsFromArrayOfConst(System::TVarRec *aArguments, const int aArguments_High);
	System::UnicodeString __fastcall DoHTTP(const System::UnicodeString aURL, System::Classes::TStringList* const aPost = (System::Classes::TStringList*)(0x0));
	void __fastcall DoAuthenticationRequestToken();
	void __fastcall DoAuthenticationSetToken();
	void __fastcall DoAuthentication();
	void __fastcall DoHTTPNegotiation();
	void __fastcall DoWebSocketURL();
	Sgcwebsocket_types::TwsSignalRCoreMessages __fastcall GetMessageType();
	
protected:
	virtual void __fastcall DoPing();
	virtual void __fastcall DoClose(const System::UnicodeString aReason);
	virtual void __fastcall DoHandshakeRequest();
	virtual void __fastcall DoInvoke(const System::UnicodeString aTarget, const System::UnicodeString aArguments = L"[]", const System::UnicodeString aInvocationId = System::UnicodeString());
	virtual bool __fastcall DoInvokeAndWait(const System::UnicodeString aTarget, const System::UnicodeString aArguments, const System::UnicodeString aInvocationId, /* out */ TSignalRCore_Completion &Completion, const int aTimeout = 0x2710);
	virtual void __fastcall DoInvokeStream(const System::UnicodeString aTarget, const System::UnicodeString aArguments = L"[]", const System::UnicodeString aInvocationId = L"0");
	virtual bool __fastcall DoInvokeStreamAndWait(const System::UnicodeString aTarget, const System::UnicodeString aArguments, const System::UnicodeString aInvocationId, /* out */ TSignalRCore_Completion &Completion, const int aTimeout = 0x2710);
	virtual void __fastcall DoCancelInvocation(const System::UnicodeString aInvocationId);
	virtual void __fastcall DoReceiveFirstMessage();
	virtual void __fastcall DoReceiveCloseMessage();
	virtual void __fastcall DoReceiveInvocationMessage();
	virtual void __fastcall DoReceiveStreamInvocationMessage();
	virtual void __fastcall DoReceiveStreamItem();
	virtual void __fastcall DoReceiveCompletionMessage();
	virtual void __fastcall DoReceiveCancelInvocationMessage();
	
public:
	void __fastcall WriteData(const System::UnicodeString aText);
	HIDESBASE void __fastcall Invoke(const System::UnicodeString aTarget, const System::TVarRec *aArguments, const int aArguments_High, const System::UnicodeString aInvocationId = System::UnicodeString());
	bool __fastcall InvokeAndWait(const System::UnicodeString aTarget, System::TVarRec *aArguments, const int aArguments_High, System::UnicodeString aInvocationId, /* out */ TSignalRCore_Completion &Completion, const int aTimeout = 0x2710);
	void __fastcall CancelInvocation(const System::UnicodeString aInvocationId);
	void __fastcall InvokeStream(const System::UnicodeString aTarget, const System::TVarRec *aArguments, const int aArguments_High, const System::UnicodeString aInvocationId);
	bool __fastcall InvokeStreamAndWait(const System::UnicodeString aTarget, const System::TVarRec *aArguments, const int aArguments_High, const System::UnicodeString aInvocationId, /* out */ TSignalRCore_Completion &Completion, const int aTimeout = 0x2710);
	void __fastcall StreamItem(const System::UnicodeString aInvocationId, const System::UnicodeString aItem);
	void __fastcall CompletionResult(const System::UnicodeString aInvocationId, const System::UnicodeString aResult);
	void __fastcall CompletionError(const System::UnicodeString aInvocationId, const System::UnicodeString aError);
	void __fastcall Ping();
	void __fastcall Close(const System::UnicodeString aReason = System::UnicodeString());
	void __fastcall ReConnect(const System::UnicodeString aConnectionId);
	
private:
	TsgcWSSignalRCoreBeforeConnectEvent FOnSignalRCoreBeforeConnect;
	TsgcWSSignalRCoreCancelInvocationEvent FOnSignalRCoreCancelInvocation;
	TsgcWSSignalRCoreCloseEvent FOnSignalRCoreClose;
	TsgcWSSignalRCoreCompletionEvent FOnSignalRCoreCompletion;
	TsgcWSSignalRCoreConnectEvent FOnSignalRCoreConnect;
	TsgcWSSignalRCoreDisconnectEvent FOnSignalRCoreDisconnect;
	TsgcWSSignalRCoreErrorEvent FOnSignalRCoreError;
	TsgcWSSignalRCoreInvocationEvent FOnSignalRCoreInvocation;
	TsgcWSSignalRCoreKeepAliveEvent FOnSignalRCoreKeepAlive;
	TsgcWSSignalRCoreStreamInvocationEvent FOnSignalRCoreStreamInvocation;
	TsgcWSSignalRCoreStreamItemEvent FOnSignalRCoreStreamItem;
	
protected:
	virtual void __fastcall DoEventSignalRCoreBeforeConnect(System::UnicodeString &ConnectionId);
	virtual void __fastcall DoEventSignalRCoreConnect(const TSignalRCore_Connect &aConnect);
	virtual void __fastcall DoEventSignalRCoreInvocation(const TSignalRCore_Invocation &aInvocation);
	virtual void __fastcall DoEventSignalRCoreStreamInvocation(const TSignalRCore_Invocation &aStreamInvocation);
	virtual void __fastcall DoEventSignalRCoreStreamItem(const TSignalRCore_StreamItem &aStreamItem);
	virtual void __fastcall DoEventSignalRCoreCompletion(const TSignalRCore_Completion &aCompletion);
	virtual void __fastcall DoEventSignalRCoreKeepAlive();
	virtual void __fastcall DoEventSignalRCoreCancelInvocation(const TSignalRCore_CancelInvocation &aCancelInvocation);
	virtual void __fastcall DoEventSignalRCoreError(const TSignalRCore_Error &aError);
	virtual void __fastcall DoEventSignalRCoreDisconnect(const int aCloseCode, const System::UnicodeString aCloseReason);
	virtual void __fastcall DoEventSignalRCoreClose(const TSignalRCore_Close &aClose);
	__property TsgcWSSignalRCoreBeforeConnectEvent OnSignalRCoreBeforeConnect = {read=FOnSignalRCoreBeforeConnect, write=FOnSignalRCoreBeforeConnect};
	__property TsgcWSSignalRCoreConnectEvent OnSignalRCoreConnect = {read=FOnSignalRCoreConnect, write=FOnSignalRCoreConnect};
	__property TsgcWSSignalRCoreInvocationEvent OnSignalRCoreInvocation = {read=FOnSignalRCoreInvocation, write=FOnSignalRCoreInvocation};
	__property TsgcWSSignalRCoreStreamInvocationEvent OnSignalRCoreStreamInvocation = {read=FOnSignalRCoreStreamInvocation, write=FOnSignalRCoreStreamInvocation};
	__property TsgcWSSignalRCoreStreamItemEvent OnSignalRCoreStreamItem = {read=FOnSignalRCoreStreamItem, write=FOnSignalRCoreStreamItem};
	__property TsgcWSSignalRCoreCompletionEvent OnSignalRCoreCompletion = {read=FOnSignalRCoreCompletion, write=FOnSignalRCoreCompletion};
	__property TsgcWSSignalRCoreCancelInvocationEvent OnSignalRCoreCancelInvocation = {read=FOnSignalRCoreCancelInvocation, write=FOnSignalRCoreCancelInvocation};
	__property TsgcWSSignalRCoreKeepAliveEvent OnSignalRCoreKeepAlive = {read=FOnSignalRCoreKeepAlive, write=FOnSignalRCoreKeepAlive};
	__property TsgcWSSignalRCoreErrorEvent OnSignalRCoreError = {read=FOnSignalRCoreError, write=FOnSignalRCoreError};
	__property TsgcWSSignalRCoreCloseEvent OnSignalRCoreClose = {read=FOnSignalRCoreClose, write=FOnSignalRCoreClose};
	__property TsgcWSSignalRCoreDisconnectEvent OnSignalRCoreDisconnect = {read=FOnSignalRCoreDisconnect, write=FOnSignalRCoreDisconnect};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoNotifyException(const System::UnicodeString Error, System::Sysutils::Exception* aException){ Sgcwebsocket_classes::TsgcWSComponent_Base::DoNotifyException(Error, aException); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_signalrcore */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_SIGNALRCORE)
using namespace Sgcwebsocket_api_signalrcore;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_signalrcoreHPP
