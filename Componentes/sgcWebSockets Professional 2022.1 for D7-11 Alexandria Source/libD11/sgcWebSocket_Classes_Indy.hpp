// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Classes_Indy.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_classes_indyHPP
#define Sgcwebsocket_classes_indyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Extensions.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <IdTCPConnection.hpp>
#include <IdHashSHA.hpp>
#include <IdGlobalProtocols.hpp>
#include <IdGlobal.hpp>
#include <IdHeaderList.hpp>
#include <IdThread.hpp>
#include <IdException.hpp>
#include <IdComponent.hpp>
#include <IdContext.hpp>
#include <IdIntercept.hpp>
#include <IdServerInterceptLogBase.hpp>
#include <IdCustomHTTPServer.hpp>
#include <IdSSL.hpp>
#include <sgcSocket_Classes.hpp>
#include <IdLogFile.hpp>
#include <IdLogBase.hpp>
#include <IdBaseComponent.hpp>
#include <IdServerInterceptLogFile.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_classes_indy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcThreadListQueuePings;
class DELPHICLASS TsgcWSMsg;
class DELPHICLASS TsgcWSThrottle;
class DELPHICLASS TsgcPing;
class DELPHICLASS TsgcWSServerForwardBase;
class DELPHICLASS TsgcWSIdLogFileClient;
class DELPHICLASS TsgcWSIdLogFileServer;
class DELPHICLASS TsgcWSIdLogFileServerConnection;
class DELPHICLASS TsgcWSServerForwardHTTP;
class DELPHICLASS TsgcWSHandshake;
class DELPHICLASS TsgcWSConnection_Base_Indy;
class DELPHICLASS TsgcWSConnection_Indy;
class DELPHICLASS TsgcWSComponent_Client_Indy;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Byte> _dt_Sgcwebsocket_classes_indy_1;
typedef void __fastcall (__closure *TsgcWSHTTP2RequestEvent)(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TStringList* const aHeaders, const _dt_Sgcwebsocket_classes_indy_1 Bytes, int aStreamIdentifierRequest, int aStreamIdentifierPush);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcThreadListQueuePings : public Sgcwebsocket_classes_syncobjs::TsgcThreadList
{
	typedef Sgcwebsocket_classes_syncobjs::TsgcThreadList inherited;
	
public:
	/* TsgcThreadList.Create */ inline __fastcall virtual TsgcThreadListQueuePings() : Sgcwebsocket_classes_syncobjs::TsgcThreadList() { }
	/* TsgcThreadList.Destroy */ inline __fastcall virtual ~TsgcThreadListQueuePings() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMsg : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FContinuation;
	bool FFragmented;
	int FMaskIndex;
	Sgcwebsocket_types::TOpcode FOpCode;
	Sgcwebsocket_types::TPayload FPayLoad;
	int FPayLoadLength;
	int FPLState;
	Idglobal::TIdBytes FReadBuffer;
	bool FReadMasked;
	int FState;
	System::Classes::TStream* FStream;
	System::Classes::TStream* __fastcall GetStream();
	
public:
	__fastcall virtual TsgcWSMsg();
	__fastcall virtual ~TsgcWSMsg();
	HIDESBASE void __fastcall Free();
	
protected:
	System::StaticArray<System::Byte, 4> Mask;
	
public:
	void __fastcall Clear();
	void __fastcall ClearStream();
	void __fastcall ClearReadBuffer();
	__property bool Continuation = {read=FContinuation, write=FContinuation, nodefault};
	__property bool Fragmented = {read=FFragmented, write=FFragmented, nodefault};
	__property int MaskIndex = {read=FMaskIndex, write=FMaskIndex, nodefault};
	__property Sgcwebsocket_types::TOpcode OpCode = {read=FOpCode, write=FOpCode, nodefault};
	__property Sgcwebsocket_types::TPayload PayLoad = {read=FPayLoad, write=FPayLoad, nodefault};
	__property int PayLoadLength = {read=FPayLoadLength, write=FPayLoadLength, nodefault};
	__property int PLState = {read=FPLState, write=FPLState, nodefault};
	__property Idglobal::TIdBytes ReadBuffer = {read=FReadBuffer, write=FReadBuffer};
	__property bool ReadMasked = {read=FReadMasked, write=FReadMasked, nodefault};
	__property int State = {read=FState, write=FState, nodefault};
	__property System::Classes::TStream* Stream = {read=GetStream, write=FStream};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSThrottle : public Sgctcp_classes::TsgcTCPThrottle
{
	typedef Sgctcp_classes::TsgcTCPThrottle inherited;
	
public:
	/* TsgcSocketThrottle.Create */ inline __fastcall virtual TsgcWSThrottle() : Sgctcp_classes::TsgcTCPThrottle() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSThrottle() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcPing : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TDateTime FDate;
	System::UnicodeString FText;
	
public:
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Create */ inline __fastcall TsgcPing() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcPing() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSServerForwardBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FEnabled;
	
public:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcWSServerForwardBase() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSServerForwardBase() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSIdLogFileClient : public Sgcsocket_classes_indy::TsgcIdLogFileClient
{
	typedef Sgcsocket_classes_indy::TsgcIdLogFileClient inherited;
	
private:
	bool FSentData;
	bool FUnMaskFrames;
	
protected:
	virtual void __fastcall InitComponent();
	virtual void __fastcall LogSentData(const System::UnicodeString AText, const System::UnicodeString AData);
	
public:
	__property bool UnMaskFrames = {read=FUnMaskFrames, write=FUnMaskFrames, nodefault};
public:
	/* TsgcIdLogFileBase.Destroy */ inline __fastcall virtual ~TsgcWSIdLogFileClient() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcWSIdLogFileClient(System::Classes::TComponent* AOwner)/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileClient(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcWSIdLogFileClient()/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileClient() { }
	
};


class PASCALIMPLEMENTATION TsgcWSIdLogFileServer : public Sgcsocket_classes_indy::TsgcIdLogFileServer
{
	typedef Sgcsocket_classes_indy::TsgcIdLogFileServer inherited;
	
private:
	bool FUnMaskFrames;
	
public:
	virtual Idintercept::TIdConnectionIntercept* __fastcall Accept(System::Classes::TComponent* aConnection);
	__property bool UnMaskFrames = {read=FUnMaskFrames, write=FUnMaskFrames, nodefault};
public:
	/* TIdServerInterceptLogFile.Destroy */ inline __fastcall virtual ~TsgcWSIdLogFileServer() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcWSIdLogFileServer(System::Classes::TComponent* AOwner)/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileServer(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcWSIdLogFileServer()/* overload */ : Sgcsocket_classes_indy::TsgcIdLogFileServer() { }
	
};


class PASCALIMPLEMENTATION TsgcWSIdLogFileServerConnection : public Idserverinterceptlogbase::TIdServerInterceptLogFileConnection
{
	typedef Idserverinterceptlogbase::TIdServerInterceptLogFileConnection inherited;
	
private:
	bool FUnMaskFrames;
	
public:
	void __fastcall LogRecvDecodedString(const System::UnicodeString AText);
	__property bool UnMaskFrames = {read=FUnMaskFrames, write=FUnMaskFrames, nodefault};
public:
	/* TIdLogBase.Destroy */ inline __fastcall virtual ~TsgcWSIdLogFileServerConnection() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcWSIdLogFileServerConnection(System::Classes::TComponent* AOwner)/* overload */ : Idserverinterceptlogbase::TIdServerInterceptLogFileConnection(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcWSIdLogFileServerConnection()/* overload */ : Idserverinterceptlogbase::TIdServerInterceptLogFileConnection() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSServerForwardHTTP : public TsgcWSServerForwardBase
{
	typedef TsgcWSServerForwardBase inherited;
	
private:
	System::UnicodeString FDocument;
	Sgctcp_classes::TsgcTCPTLS_Options* FTLSOptions;
	System::UnicodeString FURL;
	Sgctcp_classes::TsgcTCPTLS_Options* __fastcall GetTLSOptions();
	
public:
	__fastcall virtual ~TsgcWSServerForwardHTTP();
	System::UnicodeString __fastcall GetForwardURL();
	__property System::UnicodeString Document = {read=FDocument, write=FDocument};
	__property Sgctcp_classes::TsgcTCPTLS_Options* TLSOptions = {read=GetTLSOptions, write=FTLSOptions};
	__property System::UnicodeString URL = {read=FURL, write=FURL};
public:
	/* TObject.Create */ inline __fastcall TsgcWSServerForwardHTTP() : TsgcWSServerForwardBase() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHandshake : public Sgcwebsocket_classes::TsgcWSHandshake_Base
{
	typedef Sgcwebsocket_classes::TsgcWSHandshake_Base inherited;
	
protected:
	System::UnicodeString __fastcall GetWSAccept(const System::UnicodeString aKey)/* overload */;
	Idglobal::TIdBytes __fastcall GetWSAccept(System::UnicodeString aKey1, System::UnicodeString aKey2, System::UnicodeString aKey3)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TsgcWSHandshake() : Sgcwebsocket_classes::TsgcWSHandshake_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHandshake() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSConnection_Base_Indy : public Sgcwebsocket_classes::TsgcWSConnection_Abstract
{
	typedef Sgcwebsocket_classes::TsgcWSConnection_Abstract inherited;
	
protected:
	HIDESBASE bool __fastcall IsControlFrame(Sgcwebsocket_types::TOpcode aOpCode);
	virtual bool __fastcall ExistsTCPConnection() = 0 ;
	
private:
	TsgcWSMsg* FWSMessage;
	TsgcWSMsg* FWSControlCode;
	TsgcWSMsg* FWSMSG;
	int FReadBufferSize;
	TsgcWSMsg* __fastcall GetWSMessage();
	TsgcWSMsg* __fastcall GetWSControlCode();
	TsgcWSMsg* __fastcall GetWSMSG();
	void __fastcall SetWSMSG(TsgcWSMsg* const Value);
	
protected:
	__property TsgcWSMsg* WSMSG = {read=GetWSMSG, write=SetWSMSG};
	__property TsgcWSMsg* WSMessage = {read=GetWSMessage, write=FWSMessage};
	__property TsgcWSMsg* WSControlCode = {read=GetWSControlCode, write=FWSControlCode};
	__property int ReadBufferSize = {read=FReadBufferSize, write=FReadBufferSize, nodefault};
	
private:
	System::Classes::TStream* FStream;
	
protected:
	int FState;
	virtual void __fastcall DoDisconnectIfActive();
	bool FDoDisconnect;
	bool FMustDisconnect;
	virtual void __fastcall DoDisconnect(const System::UnicodeString aError = System::UnicodeString(), int aCloseCode = 0x3e8);
	
public:
	virtual void __fastcall DisconnectPeer(bool aCallOnDisconnectIfClosed = false) = 0 ;
	virtual void __fastcall Disconnect()/* overload */;
	virtual void __fastcall Disconnect(const int aCloseCode)/* overload */;
	
protected:
	virtual void __fastcall DoClearBuffer() = 0 ;
	virtual void __fastcall DoClose(int aCloseCode = 0x3e8);
	
public:
	virtual void __fastcall Close()/* overload */;
	virtual void __fastcall Close(const int aCloseCode)/* overload */;
	
private:
	TsgcThreadListQueuePings* FQueuePingList;
	TsgcThreadListQueuePings* __fastcall GetQueuePingList();
	void __fastcall ClearPingList();
	
protected:
	__property TsgcThreadListQueuePings* QueuePingList = {read=GetQueuePingList, write=FQueuePingList};
	virtual void __fastcall DoPing(const System::UnicodeString AText);
	
private:
	System::Byte __fastcall DoEncodeHeader(Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false);
	Idglobal::TIdBytes __fastcall DoEncodeMaskingKey();
	Idglobal::TIdBytes __fastcall DoEncodePayLoad(__int64 aLength);
	Idglobal::TIdBytes __fastcall DoEncodeMessage(Idglobal::TIdBytes aBytes, Idglobal::TIdBytes aBytesMsg);
	Idheaderlist::TIdHeaderList* FHeadersRequest;
	Idheaderlist::TIdHeaderList* __fastcall GetHeadersRequest();
	
protected:
	virtual void __fastcall DoHandshake() = 0 ;
	
public:
	__property Idheaderlist::TIdHeaderList* HeadersRequest = {read=GetHeadersRequest};
	
protected:
	virtual void __fastcall DoWriteData_RFC6455(const System::UnicodeString AText, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455_Fragmented(const System::UnicodeString AText, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455_Fragmented(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	virtual void __fastcall DoWriteData_Hixie76(const System::UnicodeString AText, Sgcwebsocket_types::TOpcode aOpCode);
	void __fastcall DoWriteData_SSE(const System::UnicodeString AText);
	void __fastcall DoWriteData_TCP(const System::UnicodeString AText)/* overload */;
	void __fastcall DoWriteData_TCP(System::Classes::TStream* const aStream)/* overload */;
	
private:
	TsgcWSServerForwardHTTP* FForward_HTTP;
	
protected:
	TsgcWSServerForwardHTTP* __fastcall GetForward_HTTP();
	
public:
	__property TsgcWSServerForwardHTTP* Forward_HTTP = {read=GetForward_HTTP};
	
private:
	System::UnicodeString FAltSvc;
	
protected:
	__property System::UnicodeString AltSvc = {read=FAltSvc, write=FAltSvc};
	void __fastcall DoHTTPResponse(const System::UnicodeString aContent, const System::UnicodeString aContentType, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0), int aStreamIdentifier = 0x0)/* overload */;
	virtual void __fastcall DoHTTPResponse(System::Classes::TStream* const aStream, System::UnicodeString aContentType, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0), int aStreamIdentifier = 0x0) = 0 /* overload */;
	virtual void __fastcall DoHTTPError(int aCode = 0x194, System::UnicodeString aDescription = L"Not Found") = 0 ;
	virtual void __fastcall DoHTTPRedirect(const System::UnicodeString aURL) = 0 ;
	virtual void __fastcall DoWriteData(const System::UnicodeString AText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual void __fastcall DoWriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	__fastcall virtual TsgcWSConnection_Base_Indy();
	__fastcall virtual ~TsgcWSConnection_Base_Indy();
	
private:
	bool __fastcall IsValidUTF8(Sgcbase_helpers::TsgcStringStream* aStream);
	void __fastcall DoWriteWSMSGBuffer();
	void __fastcall DoWriteWSMSGFragmented();
	
protected:
	virtual void __fastcall DoLogRead(Sgcwebsocket_types::TOpcode aOpCode, System::Classes::TStream* const aStream);
	virtual void __fastcall DoLogWrite(Sgcwebsocket_types::TOpcode aOpCode, const System::UnicodeString AText, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoLogWrite(Sgcwebsocket_types::TOpcode aOpCode, System::Classes::TStream* const aStream, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteWSMSG();
	virtual int __fastcall GetPayloadReadLength();
	virtual System::UnicodeString __fastcall DoReadUTF8(System::Classes::TStream* aStream);
	void __fastcall DoReadData_RFC6455(bool aErrorIfNotMasked = false);
	void __fastcall DoReadData_RFC8441();
	void __fastcall DoReadData_Hixie76();
	void __fastcall DoReadData_TCP();
	virtual void __fastcall ReadData(bool aErrorIfNotMasked = false) = 0 ;
};


class PASCALIMPLEMENTATION TsgcWSConnection_Indy : public TsgcWSConnection_Base_Indy
{
	typedef TsgcWSConnection_Base_Indy inherited;
	
private:
	Idtcpconnection::TIdTCPConnection* FTCPConnection;
	Idtcpconnection::TIdTCPConnection* __fastcall GetTCPConnection();
	void __fastcall SetTCPConnection(Idtcpconnection::TIdTCPConnection* const Value);
	
protected:
	__property Idtcpconnection::TIdTCPConnection* TCPConnection = {read=GetTCPConnection, write=SetTCPConnection};
	virtual bool __fastcall ExistsTCPConnection();
	virtual bool __fastcall GetActive();
	
private:
	System::UnicodeString __fastcall GetOpCodeString(Sgcwebsocket_types::TOpcode aOpCode);
	
protected:
	virtual void __fastcall DoAvoidLogStart();
	virtual void __fastcall DoAvoidLogEnd();
	virtual void __fastcall DoLogRead(Sgcwebsocket_types::TOpcode aOpCode, System::Classes::TStream* const aStream);
	virtual void __fastcall DoLogWrite(Sgcwebsocket_types::TOpcode aOpCode, const System::UnicodeString AText, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoLogWrite(Sgcwebsocket_types::TOpcode aOpCode, System::Classes::TStream* const aStream, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455(const System::UnicodeString AText, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455_Fragmented(const System::UnicodeString AText, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	virtual void __fastcall DoWriteData_RFC6455_Fragmented(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	virtual void __fastcall DoHTTPResponse(System::Classes::TStream* const aStream, System::UnicodeString aContentType, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0), int aStreamIdentifier = 0x0)/* overload */;
	virtual void __fastcall DoHTTPError(int aCode = 0x194, System::UnicodeString aDescription = L"Not Found");
	virtual void __fastcall DoHTTPRedirect(const System::UnicodeString aURL);
	virtual System::UnicodeString __fastcall GetIP();
	virtual int __fastcall GetPort();
	virtual System::UnicodeString __fastcall GetLocalIP();
	virtual int __fastcall GetLocalPort();
	virtual Sgcwebsocket_types::TwsIPVersion __fastcall GetIPVersion();
	virtual void __fastcall DoClearBuffer();
	
public:
	virtual void __fastcall DisconnectPeer(bool aCallOnDisconnectIfClosed = false);
	
protected:
	virtual void __fastcall DoWriteBytes(Idglobal::TIdBytes aBuffer);
	virtual void __fastcall DoWriteLn(const System::UnicodeString AText = System::UnicodeString());
	virtual void __fastcall DoWriteBufferOpen();
	virtual void __fastcall DoWriteBufferFlush();
	virtual void __fastcall DoWriteBufferClose();
	virtual void __fastcall DoReadBytes(Idglobal::TIdBytes &aBuffer, int aCount);
	virtual int __fastcall GetReadBufferSize();
	virtual void __fastcall DoAfterReadData();
	virtual void __fastcall ReadData(bool aErrorIfNotMasked = false);
	
public:
	__fastcall virtual TsgcWSConnection_Indy();
	__fastcall virtual ~TsgcWSConnection_Indy();
	
private:
	int FReadTimeOut;
	
public:
	__property int ReadTimeOut = {read=FReadTimeOut, write=FReadTimeOut, nodefault};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  DoHTTPResponse(const System::UnicodeString aContent, const System::UnicodeString aContentType, System::Classes::TStrings* const aCustomHeaders = (System::Classes::TStrings*)(0x0), int aStreamIdentifier = 0x0){ TsgcWSConnection_Base_Indy::DoHTTPResponse(aContent, aContentType, aCustomHeaders, aStreamIdentifier); }
	
};


class PASCALIMPLEMENTATION TsgcWSComponent_Client_Indy : public Sgcwebsocket_classes::TsgcWSComponent_WSClient
{
	typedef Sgcwebsocket_classes::TsgcWSComponent_WSClient inherited;
	
private:
	TsgcWSThrottle* FThrottle;
	
protected:
	virtual TsgcWSThrottle* __fastcall GetThrottle();
	virtual void __fastcall SetThrottle(TsgcWSThrottle* const Value);
	
public:
	__property TsgcWSThrottle* Throttle = {read=GetThrottle, write=SetThrottle};
	__fastcall virtual ~TsgcWSComponent_Client_Indy();
public:
	/* TsgcWSComponent_WSClient.Create */ inline __fastcall virtual TsgcWSComponent_Client_Indy(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSComponent_WSClient(aOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_classes_indy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLASSES_INDY)
using namespace Sgcwebsocket_classes_indy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_classes_indyHPP
