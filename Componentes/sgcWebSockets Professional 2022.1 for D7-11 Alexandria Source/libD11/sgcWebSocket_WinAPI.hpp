// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_WinAPI.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_winapiHPP
#define Sgcwebsocket_winapiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_winapi
{
//-- forward type declarations -----------------------------------------------
struct WINHTTP_PROXY_INFO;
struct WINHTTP_ASYNC_RESULT;
struct WINHTTP_WEB_SOCKET_ASYNC_RESULT;
struct WINHTTP_WEB_SOCKET_STATUS;
//-- type declarations -------------------------------------------------------
typedef PWORD PUSHORT;

struct DECLSPEC_DRECORD WINHTTP_PROXY_INFO
{
public:
	unsigned dwAccessType;
	System::WideChar *lpszProxy;
	System::WideChar *lpszProxyBypass;
};


typedef WINHTTP_PROXY_INFO *LPWINHTTP_PROXY_INFO;

typedef WINHTTP_PROXY_INFO *PWINHTTP_PROXY_INFO;

typedef WINHTTP_PROXY_INFO TWinHttpProxyInfo;

typedef WINHTTP_PROXY_INFO *PWinHttpProxyInfo;

typedef void __stdcall (*WINHTTP_STATUS_CALLBACK)(void * hInternet, void * dwContext, unsigned dwInternetStatus, void * lpvStatusInformation, unsigned dwStatusInformationLength);

typedef WINHTTP_STATUS_CALLBACK *LPWINHTTP_STATUS_CALLBACK;

typedef WINHTTP_STATUS_CALLBACK TWinHttpStatusCallback;

typedef WINHTTP_STATUS_CALLBACK *PWinHttpStatusCallback;

struct DECLSPEC_DRECORD WINHTTP_ASYNC_RESULT
{
public:
	NativeUInt dwResult;
	unsigned dwError;
};


typedef WINHTTP_ASYNC_RESULT *LPWINHTTP_ASYNC_RESULT;

typedef WINHTTP_ASYNC_RESULT TWinHttpAsyncResult;

typedef WINHTTP_ASYNC_RESULT *PWinHttpAsyncResult;

enum DECLSPEC_DENUM WINHTTP_WEB_SOCKET_OPERATION : unsigned char { WINHTTP_WEB_SOCKET_SEND_OPERATION, WINHTTP_WEB_SOCKET_RECEIVE_OPERATION, WINHTTP_WEB_SOCKET_CLOSE_OPERATION, WINHTTP_WEB_SOCKET_SHUTDOWN_OPERATION };

typedef WINHTTP_WEB_SOCKET_OPERATION *PWINHTTP_WEB_SOCKET_OPERATION;

struct DECLSPEC_DRECORD WINHTTP_WEB_SOCKET_ASYNC_RESULT
{
public:
	WINHTTP_ASYNC_RESULT AsyncResult;
	WINHTTP_WEB_SOCKET_OPERATION Operation;
};


typedef WINHTTP_WEB_SOCKET_ASYNC_RESULT *LPWINHTTP_WEB_SOCKET_ASYNC_RESULT;

typedef WINHTTP_WEB_SOCKET_ASYNC_RESULT TWinHttpWebSocketAsyncResult;

typedef WINHTTP_WEB_SOCKET_ASYNC_RESULT *PWinHttpWebSocketAsyncResult;

enum DECLSPEC_DENUM WINHTTP_WEB_SOCKET_BUFFER_TYPE : unsigned char { WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE, WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE, WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE, WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE, WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE };

typedef WINHTTP_WEB_SOCKET_BUFFER_TYPE *PWINHTTP_WEB_SOCKET_BUFFER_TYPE;

struct DECLSPEC_DRECORD WINHTTP_WEB_SOCKET_STATUS
{
public:
	unsigned dwBytesTransferred;
	WINHTTP_WEB_SOCKET_BUFFER_TYPE eBufferType;
};


typedef WINHTTP_WEB_SOCKET_STATUS *LPWINHTTP_WEB_SOCKET_STATUS;

typedef WINHTTP_WEB_SOCKET_STATUS TWinHttpWebSocketStatus;

typedef WINHTTP_WEB_SOCKET_STATUS *PWinHttpWebSocketStatus;

enum DECLSPEC_DENUM WINHTTP_WEB_SOCKET_CLOSE_STATUS : unsigned short { WINHTTP_WEB_SOCKET_SUCCESS_CLOSE_STATUS = 1000, WINHTTP_WEB_SOCKET_ENDPOINT_TERMINATED_CLOSE_STATUS, WINHTTP_WEB_SOCKET_PROTOCOL_ERROR_CLOSE_STATUS, WINHTTP_WEB_SOCKET_INVALID_DATA_TYPE_CLOSE_STATUS, WINHTTP_WEB_SOCKET_EMPTY_CLOSE_STATUS = 1005, WINHTTP_WEB_SOCKET_ABORTED_CLOSE_STATUS, WINHTTP_WEB_SOCKET_INVALID_UTF8_CLOSE_STATUS, WINHTTP_WEB_SOCKET_POLICY_VIOLATION_CLOSE_STATUS, WINHTTP_WEB_SOCKET_MESSAGE_TOO_BIG_CLOSE_STATUS, WINHTTP_WEB_SOCKET_UNSUPPORTED_EXTENSIONS_CLOSE_STATUS, WINHTTP_WEB_SOCKET_SERVER_ERROR_CLOSE_STATUS, WINHTTP_WEB_SOCKET_SECURE_HANDSHAKE_ERROR_CLOSE_STATUS = 1015 };

typedef WINHTTP_WEB_SOCKET_CLOSE_STATUS *PWINHTTP_WEB_SOCKET_CLOSE_STATUS;

typedef System::LongBool __stdcall (*TsgcWinHttpCheckPlatform)(void);

typedef System::LongBool __stdcall (*TsgcWinHttpCloseHandle)(void * hInternet);

typedef void * __stdcall (*TsgcWinHttpOpen)(System::WideChar * pszAgentW, unsigned dwAccessType, System::WideChar * pszProxyW, System::WideChar * pszProxyBypassW, unsigned dwFlags);

typedef void * __stdcall (*TsgcWinHttpConnect)(void * hSession, System::WideChar * pswzServerName, System::Word nServerPort, unsigned dwReserved);

typedef void * __stdcall (*TsgcWinHttpOpenRequest)(void * hConnect, System::WideChar * pwszVerb, System::WideChar * pwszObjectName, System::WideChar * pwszVersion, System::WideChar * pwszReferrer, Winapi::Windows::PLPWSTR ppwszAcceptTypes, unsigned dwFlags);

typedef System::LongBool __stdcall (*TsgcWinHttpSetTimeouts)(void * hInternet, int nResolveTimeout, int nConnectTimeout, int nSendTimeout, int nReceiveTimeout);

typedef System::LongBool __stdcall (*TsgcWinHttpSetOption)(void * hInternet, unsigned dwOption, void * lpBuffer, unsigned dwBufferLength);

typedef System::LongBool __stdcall (*TsgcWinHttpSetCredentials)(void * hRequest, unsigned AuthTargets, unsigned AuthScheme, System::WideChar * pwszUserName, System::WideChar * pwszPassword, void * pAuthParams);

typedef System::LongBool __stdcall (*TsgcWinHttpAddRequestHeaders)(void * hRequest, System::WideChar * pwszHeaders, unsigned dwHeadersLength, unsigned dwModifiers);

typedef System::LongBool __stdcall (*TsgcWinHttpSendRequest)(void * hRequest, System::WideChar * lpszHeaders, unsigned dwHeadersLength, void * lpOptional, unsigned dwOptionalLength, unsigned dwTotalLength, NativeUInt dwContext);

typedef System::LongBool __stdcall (*TsgcWinHttpReceiveResponse)(void * hRequest, void * lpReserved);

typedef System::LongBool __stdcall (*TsgcWinHttpQueryHeaders)(void * hRequest, unsigned dwInfoLevel, System::WideChar * pwszName, void * lpBuffer, unsigned* lpdwBufferLength, unsigned* lpdwIndex);

typedef WINHTTP_STATUS_CALLBACK __stdcall (*TsgcWinHttpSetStatusCallback)(void * hInternet, WINHTTP_STATUS_CALLBACK lpfnInternetCallback, unsigned dwNotificationFlags, NativeUInt dwReserved);

typedef unsigned __stdcall (*TsgcWinHttpWebSocketClose)(void * hWebSocket, System::Word usStatus, void * pvReason, unsigned dwReasonLength);

typedef void * __stdcall (*TsgcWinHttpWebSocketCompleteUpgrade)(void * hRequest, NativeUInt pContext);

typedef unsigned __stdcall (*TsgcWinHttpWebSocketQueryCloseStatus)(void * hRequest, PWINHTTP_WEB_SOCKET_CLOSE_STATUS pusStatus, void * pvReason, unsigned dwReasonLength, unsigned* pdwReasonLengthConsumed);

typedef unsigned __stdcall (*TsgcWinHttpWebSocketReceive)(void * hWebSocket, void * pvBuffer, unsigned dwBufferLength, void * pdwBytesRead, void * peBufferType);

typedef unsigned __stdcall (*TsgcWinHttpWebSocketSend)(void * hWebSocket, int eBufferType, void * pvBuffer, unsigned dwBufferLength);

typedef unsigned __stdcall (*TsgcWinHttpWebSocketShutdown)(void * hWebSocket, System::Word usStatus, void * pvReason, unsigned dwReasonLength);

//-- var, const, procedure ---------------------------------------------------
static const System::Word ERROR_WINHTTP_INVALID_OPERATION = System::Word(0x10dd);
static const System::Int8 WINHTTP_ACCESS_TYPE_NAMED_PROXY = System::Int8(0x3);
static const System::Int8 WINHTTP_OPTION_PROXY = System::Int8(0x26);
static const System::Int8 WINHTTP_AUTH_TARGET_PROXY = System::Int8(0x1);
static const System::Int8 WINHTTP_AUTH_SCHEME_BASIC = System::Int8(0x1);
static const System::Int8 WINHTTP_ACCESS_TYPE_NO_PROXY = System::Int8(0x1);
#define WINHTTP_NO_PROXY_NAME (void*)(0)
#define WINHTTP_NO_PROXY_BYPASS (void*)(0)
static const int WINHTTP_FLAG_SECURE = int(0x800000);
static const int WINHTTP_FLAG_ASYNC = int(0x10000000);
#define WINHTTP_NO_REFERER (void*)(0)
#define WINHTTP_DEFAULT_ACCEPT_TYPES (void*)(0)
static const System::Int8 WINHTTP_DISABLE_REDIRECTS = System::Int8(0x2);
static const System::Int8 WINHTTP_OPTION_DISABLE_FEATURE = System::Int8(0x3f);
static const System::Int8 WINHTTP_DISABLE_COOKIES = System::Int8(0x1);
static const int WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA = int(0x40000000);
#define WINHTTP_NO_ADDITIONAL_HEADERS (void*)(0)
#define WINHTTP_NO_REQUEST_DATA (void*)(0)
static const System::Word ERROR_WINHTTP_SECURE_FAILURE = System::Word(0x2f8f);
static const System::Word ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = System::Word(0x2f0c);
static const System::Word ERROR_WINHTTP_OPERATION_CANCELLED = System::Word(0x2ef1);
static const System::Word ERROR_WINHTTP_CANNOT_CONNECT = System::Word(0x2efd);
static const System::Word ERROR_WINHTTP_CONNECTION_ERROR = System::Word(0x2efe);
static const System::Word ERROR_WINHTTP_INVALID_SERVER_RESPONSE = System::Word(0x2f78);
static const System::Word WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA = System::Word(0x100);
static const System::Word WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = System::Word(0x2000);
static const System::Word WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID = System::Word(0x1000);
static const System::Word WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = System::Word(0x200);
static const System::Int8 WINHTTP_OPTION_SECURITY_FLAGS = System::Int8(0x1f);
static const System::Int8 WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = System::Int8(0x1);
static const System::Int8 WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = System::Int8(0x2);
static const System::Int8 WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = System::Int8(0x4);
static const System::Int8 WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = System::Int8(0x8);
static const System::Int8 WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = System::Int8(0x10);
static const System::Int8 WINHTTP_CALLBACK_STATUS_REQUEST_SENT = System::Int8(0x20);
static const System::Int8 WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = System::Int8(0x40);
static const System::Byte WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = System::Byte(0x80);
static const System::Word WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = System::Word(0x100);
static const System::Word WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = System::Word(0x200);
static const System::Word WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = System::Word(0x400);
static const System::Word WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = System::Word(0x800);
static const System::Word WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = System::Word(0x1000);
static const System::Word WINHTTP_CALLBACK_STATUS_REDIRECT = System::Word(0x4000);
static const System::Word WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = System::Word(0x8000);
static const int WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = int(0x10000);
static const int WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = int(0x20000);
static const int WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = int(0x40000);
static const int WINHTTP_CALLBACK_STATUS_READ_COMPLETE = int(0x80000);
static const int WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = int(0x100000);
static const int WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = int(0x200000);
static const int WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = int(0x400000);
static const int WINHTTP_CALLBACK_STATUS_GETPROXYFORURL_COMPLETE = int(0x1000000);
static const int WINHTTP_CALLBACK_STATUS_CLOSE_COMPLETE = int(0x2000000);
static const int WINHTTP_CALLBACK_STATUS_SHUTDOWN_COMPLETE = int(0x4000000);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED = System::Int8(0x1);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT = System::Int8(0x2);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED = System::Int8(0x4);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA = System::Int8(0x8);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID = System::Int8(0x10);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID = System::Int8(0x20);
static const System::Int8 WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE = System::Int8(0x40);
static const unsigned WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR = unsigned(0x80000000);
static const System::Int8 WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 = System::Int8(0x8);
static const System::Int8 WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 = System::Int8(0x20);
static const System::Byte WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 = System::Byte(0x80);
static const System::Word WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = System::Word(0x200);
static const System::Word WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = System::Word(0x800);
static const System::Word WINHTTP_FLAG_SECURE_PROTOCOL_ALL = System::Word(0xa80);
static const System::Int8 API_RECEIVE_RESPONSE = System::Int8(0x1);
#define S_API_RECEIVE_RESPONSE L"Receive Response"
static const System::Int8 API_QUERY_DATA_AVAILABLE = System::Int8(0x2);
#define S_API_QUERY_DATA_AVAILABLE L"Query Data Available"
static const System::Int8 API_READ_DATA = System::Int8(0x3);
#define S_API_READ_DATA L"Read Data"
static const System::Int8 API_WRITE_DATA = System::Int8(0x4);
#define S_API_WRITE_DATA L"Write Data"
static const System::Int8 API_SEND_REQUEST = System::Int8(0x5);
#define S_API_SEND_REQUEST L"Send Request"
static const System::Int8 WINHTTP_CALLBACK_FLAG_RESOLVE_NAME = System::Int8(0x3);
static const System::Int8 WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER = System::Int8(0xc);
static const System::Int8 WINHTTP_CALLBACK_FLAG_SEND_REQUEST = System::Int8(0x30);
static const System::Byte WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE = System::Byte(0xc0);
static const System::Word WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION = System::Word(0x300);
static const System::Word WINHTTP_CALLBACK_FLAG_HANDLES = System::Word(0xc00);
static const System::Word WINHTTP_CALLBACK_FLAG_DETECTING_PROXY = System::Word(0x1000);
static const System::Word WINHTTP_CALLBACK_FLAG_REDIRECT = System::Word(0x4000);
static const System::Word WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE = System::Word(0x8000);
static const int WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = int(0x10000);
static const int WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE = int(0x400000);
static const int WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = int(0x20000);
static const int WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = int(0x40000);
static const int WINHTTP_CALLBACK_FLAG_READ_COMPLETE = int(0x80000);
static const int WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = int(0x100000);
static const int WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = int(0x200000);
static const int WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS = int(0x7e0000);
static const unsigned WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = unsigned(0xffffffff);
static const System::Int8 WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = System::Int8(0x72);
static const System::Int8 WINHTTP_OPTION_WEB_SOCKET_CLOSE_TIMEOUT = System::Int8(0x73);
static const System::Int8 WINHTTP_OPTION_WEB_SOCKET_KEEPALIVE_INTERVAL = System::Int8(0x74);
static const System::Int8 WINHTTP_OPTION_DECOMPRESSION = System::Int8(0x75);
static const System::Int8 WINHTTP_OPTION_WEB_SOCKET_RECEIVE_BUFFER_SIZE = System::Int8(0x7a);
static const System::Int8 WINHTTP_OPTION_WEB_SOCKET_SEND_BUFFER_SIZE = System::Int8(0x7b);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpCheckPlatform(void);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpCloseHandle(void * hInternet);
extern DELPHI_PACKAGE void * __stdcall WinHttpOpen(System::WideChar * pszAgentW, unsigned dwAccessType, System::WideChar * pszProxyW, System::WideChar * pszProxyBypassW, unsigned dwFlags);
extern DELPHI_PACKAGE void * __stdcall WinHttpConnect(void * hSession, System::WideChar * pswzServerName, System::Word nServerPort, unsigned dwReserved);
extern DELPHI_PACKAGE void * __stdcall WinHttpOpenRequest(void * hConnect, System::WideChar * pwszVerb, System::WideChar * pwszObjectName, System::WideChar * pwszVersion, System::WideChar * pwszReferrer, Winapi::Windows::PLPWSTR ppwszAcceptTypes, unsigned dwFlags);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpSetTimeouts(void * hInternet, int nResolveTimeout, int nConnectTimeout, int nSendTimeout, int nReceiveTimeout);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpSetOption(void * hInternet, unsigned dwOption, void * lpBuffer, unsigned dwBufferLength);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpSetCredentials(void * hRequest, unsigned AuthTargets, unsigned AuthScheme, System::WideChar * pwszUserName, System::WideChar * pwszPassword, void * pAuthParams);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpAddRequestHeaders(void * hRequest, System::WideChar * pwszHeaders, unsigned dwHeadersLength, unsigned dwModifiers);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpSendRequest(void * hRequest, System::WideChar * lpszHeaders, unsigned dwHeadersLength, void * lpOptional, unsigned dwOptionalLength, unsigned dwTotalLength, NativeUInt dwContext);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpReceiveResponse(void * hRequest, void * lpReserved);
extern DELPHI_PACKAGE System::LongBool __stdcall WinHttpQueryHeaders(void * hRequest, unsigned dwInfoLevel, System::WideChar * pwszName, void * lpBuffer, unsigned* lpdwBufferLength, unsigned* lpdwIndex);
extern DELPHI_PACKAGE WINHTTP_STATUS_CALLBACK __stdcall WinHttpSetStatusCallback(void * hInternet, WINHTTP_STATUS_CALLBACK lpfnInternetCallback, unsigned dwNotificationFlags, NativeUInt dwReserved);
extern DELPHI_PACKAGE unsigned __stdcall WinHttpWebSocketClose(void * hWebSocket, System::Word usStatus, void * pvReason, unsigned dwReasonLength);
extern DELPHI_PACKAGE void * __stdcall WinHttpWebSocketCompleteUpgrade(void * hRequest, NativeUInt pContext);
extern DELPHI_PACKAGE unsigned __stdcall WinHttpWebSocketQueryCloseStatus(void * hRequest, PWINHTTP_WEB_SOCKET_CLOSE_STATUS pusStatus, void * pvReason, unsigned dwReasonLength, unsigned* pdwReasonLengthConsumed);
extern DELPHI_PACKAGE unsigned __stdcall WinHttpWebSocketReceive(void * hWebSocket, void * pvBuffer, unsigned dwBufferLength, void * pdwBytesRead, void * peBufferType);
extern DELPHI_PACKAGE unsigned __stdcall WinHttpWebSocketSend(void * hWebSocket, WINHTTP_WEB_SOCKET_BUFFER_TYPE eBufferType, void * pvBuffer, unsigned dwBufferLength);
extern DELPHI_PACKAGE unsigned __stdcall WinHttpWebSocketShutdown(void * hWebSocket, System::Word usStatus, void * pvReason, unsigned dwReasonLength);
}	/* namespace Sgcwebsocket_winapi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_WINAPI)
using namespace Sgcwebsocket_winapi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_winapiHPP
