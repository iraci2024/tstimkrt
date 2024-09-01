{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_WinAPI;

interface

{$I sgcVer.inc}

{$IFDEF SGC_WINHTTP}
Uses
  Windows, SysUtils;

type
  HINTERNET = Pointer;
  {$EXTERNALSYM HINTERNET}
  PUSHORT = PWord;

  {$IFDEF D7}
  ULONG_PTR = Cardinal;
  DWORD_PTR = ULONG_PTR;
  {$ENDIF}

  {$IFNDEF DXE}
    USHORT = Word;
    {$NODEFINE USHORT}
    PVOID = Pointer;
    {$NODEFINE PVOID}
    LPVOID = Pointer;
    {$NODEFINE LPVOID}
  {$ENDIF}

type
  INTERNET_PORT = Word;
  {$EXTERNALSYM INTERNET_PORT}
  WINHTTP_PROXY_INFO = record
    dwAccessType: DWORD;
    lpszProxy: LPWSTR;
    lpszProxyBypass: LPWSTR;
  end;
  LPWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;
  PWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;
  TWinHttpProxyInfo = WINHTTP_PROXY_INFO;
  PWinHttpProxyInfo = ^TWinHttpProxyInfo;

const
 ERROR_WINHTTP_INVALID_OPERATION = 4317;

const
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_OPTION_PROXY = 38;
  WINHTTP_AUTH_TARGET_PROXY = $00000001;
  WINHTTP_AUTH_SCHEME_BASIC = $00000001;

const
  WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
  WINHTTP_NO_PROXY_NAME = nil;
  WINHTTP_NO_PROXY_BYPASS = nil;
  WINHTTP_FLAG_SECURE = $00800000;
  WINHTTP_FLAG_ASYNC = $10000000;
  WINHTTP_NO_REFERER = nil;
  WINHTTP_DEFAULT_ACCEPT_TYPES = nil;
  WINHTTP_DISABLE_REDIRECTS = $00000002;
  WINHTTP_OPTION_DISABLE_FEATURE = 63;
  WINHTTP_DISABLE_COOKIES = $00000001;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA = $40000000;
  WINHTTP_NO_ADDITIONAL_HEADERS = nil;
  WINHTTP_NO_REQUEST_DATA = nil;
  ERROR_WINHTTP_SECURE_FAILURE = 12175;
  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = 12044;
  ERROR_WINHTTP_OPERATION_CANCELLED = 12017;
  ERROR_WINHTTP_CANNOT_CONNECT = 12029;
  ERROR_WINHTTP_CONNECTION_ERROR = 12030;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;

const
  WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = $00002000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID = $00001000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = $00000200;
  WINHTTP_OPTION_SECURITY_FLAGS = 31;

const
  WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = $00000001;
  WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = $00000002;
  WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = $00000004;
  WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = $00000008;
  WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = $00000010;
  WINHTTP_CALLBACK_STATUS_REQUEST_SENT = $00000020;
  WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = $00000040;
  WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = $00000080;
  WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = $00000100;
  WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = $00000200;
  WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = $00000400;
  WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = $00000800;
  WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = $00001000;
  WINHTTP_CALLBACK_STATUS_REDIRECT = $00004000;
  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = $00008000;
  WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = $00010000;
  WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = $00020000;
  WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = $00040000;
  WINHTTP_CALLBACK_STATUS_READ_COMPLETE = $00080000;
  WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = $00100000;
  WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = $00200000;
  WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = $00400000;

  WINHTTP_CALLBACK_STATUS_GETPROXYFORURL_COMPLETE = $01000000;
  WINHTTP_CALLBACK_STATUS_CLOSE_COMPLETE          = $02000000;
  WINHTTP_CALLBACK_STATUS_SHUTDOWN_COMPLETE       = $04000000;

const
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED         = $00000001;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT            = $00000002;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED            = $00000004;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA              = $00000008;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID         = $00000010;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID       = $00000020;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE        = $00000040;
  WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR  = $80000000;

const
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;
  WINHTTP_FLAG_SECURE_PROTOCOL_ALL    = (WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 or
                                                 WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or
                                                 WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2);





{ API Enums for WINHTTP_CALLBACK_STATUS_REQUEST_ERROR: }
  API_RECEIVE_RESPONSE = (1);
  S_API_RECEIVE_RESPONSE = 'Receive Response';

  API_QUERY_DATA_AVAILABLE = (2);
  S_API_QUERY_DATA_AVAILABLE = 'Query Data Available';

  API_READ_DATA = (3);
  S_API_READ_DATA = 'Read Data';

  API_WRITE_DATA = (4);
  S_API_WRITE_DATA = 'Write Data';

  API_SEND_REQUEST = (5);
  S_API_SEND_REQUEST = 'Send Request';


  WINHTTP_CALLBACK_FLAG_RESOLVE_NAME = WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED;
  WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER = WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER;
  WINHTTP_CALLBACK_FLAG_SEND_REQUEST = WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or WINHTTP_CALLBACK_STATUS_REQUEST_SENT;
  WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE = WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED;
  WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION = WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED;
  WINHTTP_CALLBACK_FLAG_HANDLES = WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING;
  WINHTTP_CALLBACK_FLAG_DETECTING_PROXY = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
  WINHTTP_CALLBACK_FLAG_REDIRECT = WINHTTP_CALLBACK_STATUS_REDIRECT;
  WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE = WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
  WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
  WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
  WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_READ_COMPLETE = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
  WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
  WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;


  WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
                                                        or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
                                                        or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;
  WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = $ffffffff;

type
  LPWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;
  WINHTTP_STATUS_CALLBACK = procedure(hInternet: HINTERNET; dwContext: Pointer; dwInternetStatus: DWORD;
    lpvStatusInformation: Pointer; dwStatusInformationLength: DWORD); stdcall;
  TWinHttpStatusCallback = WINHTTP_STATUS_CALLBACK;
  PWinHttpStatusCallback = ^TWinHttpStatusCallback;

type
  WINHTTP_ASYNC_RESULT = record
    dwResult: DWORD_PTR; { indicates which async API has encountered an error }
    dwError: DWORD;      { the error code if the API failed }
  end;

  LPWINHTTP_ASYNC_RESULT = ^WINHTTP_ASYNC_RESULT;
  TWinHttpAsyncResult = WINHTTP_ASYNC_RESULT;
  PWinHttpAsyncResult = ^TWinHttpAsyncResult;

type
  WINHTTP_WEB_SOCKET_OPERATION = (
    WINHTTP_WEB_SOCKET_SEND_OPERATION = 0,
    WINHTTP_WEB_SOCKET_RECEIVE_OPERATION = 1,
    WINHTTP_WEB_SOCKET_CLOSE_OPERATION = 2,
    WINHTTP_WEB_SOCKET_SHUTDOWN_OPERATION = 3);
  PWINHTTP_WEB_SOCKET_OPERATION = ^WINHTTP_WEB_SOCKET_OPERATION;

type
  WINHTTP_WEB_SOCKET_ASYNC_RESULT = record
    AsyncResult: WINHTTP_ASYNC_RESULT; { The result of a WebSocket operation. }
    Operation: WINHTTP_WEB_SOCKET_OPERATION;      { The type of WebSocket operation. }
  end;
  LPWINHTTP_WEB_SOCKET_ASYNC_RESULT = ^WINHTTP_WEB_SOCKET_ASYNC_RESULT;
  TWinHttpWebSocketAsyncResult = WINHTTP_WEB_SOCKET_ASYNC_RESULT;
  PWinHttpWebSocketAsyncResult = ^TWinHttpWebSocketAsyncResult;


type
  WINHTTP_WEB_SOCKET_BUFFER_TYPE = (
    WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE = 0,
    WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE = 1,
    WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE = 2,
    WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE = 3,
    WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE = 4);
  PWINHTTP_WEB_SOCKET_BUFFER_TYPE = ^WINHTTP_WEB_SOCKET_BUFFER_TYPE;

type
  WINHTTP_WEB_SOCKET_STATUS = record
    dwBytesTransferred: DWORD; { The amount of bytes transferred in the operation. }
    eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;      { The type of data in the buffer. }
  end;
  LPWINHTTP_WEB_SOCKET_STATUS = ^WINHTTP_WEB_SOCKET_STATUS;
  TWinHttpWebSocketStatus = WINHTTP_WEB_SOCKET_STATUS;
  PWinHttpWebSocketStatus = ^TWinHttpWebSocketStatus;

Const
  WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;
  WINHTTP_OPTION_WEB_SOCKET_CLOSE_TIMEOUT = 115;
  WINHTTP_OPTION_WEB_SOCKET_KEEPALIVE_INTERVAL = 116;
  WINHTTP_OPTION_DECOMPRESSION = 117;
  WINHTTP_OPTION_WEB_SOCKET_RECEIVE_BUFFER_SIZE = 122;
  WINHTTP_OPTION_WEB_SOCKET_SEND_BUFFER_SIZE = 123;

type

  WINHTTP_WEB_SOCKET_CLOSE_STATUS = (
    WINHTTP_WEB_SOCKET_SUCCESS_CLOSE_STATUS = 1000,
    WINHTTP_WEB_SOCKET_ENDPOINT_TERMINATED_CLOSE_STATUS = 1001,
    WINHTTP_WEB_SOCKET_PROTOCOL_ERROR_CLOSE_STATUS = 1002,
    WINHTTP_WEB_SOCKET_INVALID_DATA_TYPE_CLOSE_STATUS = 1003,
    WINHTTP_WEB_SOCKET_EMPTY_CLOSE_STATUS = 1005,
    WINHTTP_WEB_SOCKET_ABORTED_CLOSE_STATUS = 1006,
    WINHTTP_WEB_SOCKET_INVALID_UTF8_CLOSE_STATUS = 1007,
    WINHTTP_WEB_SOCKET_POLICY_VIOLATION_CLOSE_STATUS = 1008,
    WINHTTP_WEB_SOCKET_MESSAGE_TOO_BIG_CLOSE_STATUS = 1009,
    WINHTTP_WEB_SOCKET_UNSUPPORTED_EXTENSIONS_CLOSE_STATUS = 1010,
    WINHTTP_WEB_SOCKET_SERVER_ERROR_CLOSE_STATUS = 1011,
    WINHTTP_WEB_SOCKET_SECURE_HANDSHAKE_ERROR_CLOSE_STATUS = 1015
  );
  PWINHTTP_WEB_SOCKET_CLOSE_STATUS = ^WINHTTP_WEB_SOCKET_CLOSE_STATUS;


Type
TsgcWinHttpCheckPlatform = function: BOOL; stdcall;
function WinHttpCheckPlatform: BOOL; stdcall;

Type
TsgcWinHttpCloseHandle = function(hInternet: HINTERNET): BOOL; stdcall;
function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall;

Type
TsgcWinHttpOpen = function(pszAgentW: LPCWSTR; dwAccessType: DWORD; pszProxyW: LPCWSTR; pszProxyBypassW: LPCWSTR;
  dwFlags: DWORD): HINTERNET; stdcall;
function WinHttpOpen(pszAgentW: LPCWSTR; dwAccessType: DWORD; pszProxyW: LPCWSTR; pszProxyBypassW: LPCWSTR;
  dwFlags: DWORD): HINTERNET; stdcall;

Type
TsgcWinHttpConnect = function(hSession: HINTERNET; pswzServerName: LPCWSTR; nServerPort: INTERNET_PORT;
  dwReserved: DWORD): HINTERNET; stdcall;
function WinHttpConnect(hSession: HINTERNET; pswzServerName: LPCWSTR; nServerPort: INTERNET_PORT;
  dwReserved: DWORD): HINTERNET; stdcall;

Type
TsgcWinHttpOpenRequest = function(hConnect: HINTERNET; pwszVerb: LPCWSTR; pwszObjectName: LPCWSTR; pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR; ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;
function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb: LPCWSTR; pwszObjectName: LPCWSTR; pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR; ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;

Type
TsgcWinHttpSetTimeouts = function(hInternet: HINTERNET; nResolveTimeout: Integer; nConnectTimeout: Integer;
  nSendTimeout: Integer; nReceiveTimeout: Integer): BOOL; stdcall;
function WinHttpSetTimeouts(hInternet: HINTERNET; nResolveTimeout: Integer; nConnectTimeout: Integer;
  nSendTimeout: Integer; nReceiveTimeout: Integer): BOOL; stdcall;

Type
TsgcWinHttpSetOption = function(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;

Type
TsgcWinHttpSetCredentials = function(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD; pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR; pAuthParams: Pointer): BOOL; stdcall;
function WinHttpSetCredentials(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD; pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR; pAuthParams: Pointer): BOOL; stdcall;

Type
TsgcWinHttpAddRequestHeaders = function(hRequest: HINTERNET; pwszHeaders: LPCWSTR; dwHeadersLength: DWORD;
  dwModifiers: DWORD): BOOL; stdcall;
function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: LPCWSTR; dwHeadersLength: DWORD;
  dwModifiers: DWORD): BOOL; stdcall;

Type
TsgcWinHttpSendRequest = function(hRequest: HINTERNET; lpszHeaders: LPCWSTR;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD;
  dwTotalLength: DWORD; dwContext: DWORD_PTR): BOOL; stdcall;
function WinHttpSendRequest(hRequest: HINTERNET; lpszHeaders: LPCWSTR;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD;
  dwTotalLength: DWORD; dwContext: DWORD_PTR): BOOL; stdcall;

Type
TsgcWinHttpReceiveResponse = function(hRequest: HINTERNET; lpReserved: Pointer): BOOL; stdcall;
function WinHttpReceiveResponse(hRequest: HINTERNET; lpReserved: Pointer): BOOL; stdcall;

Type
TsgcWinHttpQueryHeaders = function(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: LPCWSTR; lpBuffer: LPVOID; lpdwBufferLength: LPDWORD; lpdwIndex: LPDWORD): BOOL; stdcall;
function WinHttpQueryHeaders(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: LPCWSTR; lpBuffer: LPVOID; lpdwBufferLength: LPDWORD; lpdwIndex: LPDWORD): BOOL; stdcall;

Type
TsgcWinHttpSetStatusCallback = function(hInternet: HINTERNET; lpfnInternetCallback: TWinHttpStatusCallback; dwNotificationFlags: DWORD; dwReserved: DWORD_PTR): TWinHttpStatusCallback; stdcall;
function WinHttpSetStatusCallback(hInternet: HINTERNET; lpfnInternetCallback: TWinHttpStatusCallback; dwNotificationFlags: DWORD; dwReserved: DWORD_PTR): TWinHttpStatusCallback; stdcall;




Type
TsgcWinHttpWebSocketClose = function(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;
function WinHttpWebSocketClose(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;

Type
TsgcWinHttpWebSocketCompleteUpgrade = function(hRequest: HINTERNET; pContext: DWORD_PTR): HINTERNET; stdcall;
function WinHttpWebSocketCompleteUpgrade(hRequest: HINTERNET; pContext: DWORD_PTR): HINTERNET; stdcall;

Type
TsgcWinHttpWebSocketQueryCloseStatus = function(hRequest: HINTERNET; pusStatus: PWINHTTP_WEB_SOCKET_CLOSE_STATUS; pvReason: PVOID; dwReasonLength: DWORD; pdwReasonLengthConsumed: PDWORD): DWORD; stdcall;
function WinHttpWebSocketQueryCloseStatus(hRequest: HINTERNET; pusStatus: PWINHTTP_WEB_SOCKET_CLOSE_STATUS; pvReason: PVOID; dwReasonLength: DWORD; pdwReasonLengthConsumed: PDWORD): DWORD; stdcall;

Type
TsgcWinHttpWebSocketReceive = function(hWebSocket: HINTERNET; pvBuffer: PVOID; dwBufferLength: DWORD; pdwBytesRead: Pointer; peBufferType: Pointer): DWORD; stdcall;
function WinHttpWebSocketReceive(hWebSocket: HINTERNET; pvBuffer: PVOID; dwBufferLength: DWORD; pdwBytesRead: Pointer; peBufferType: Pointer): DWORD; stdcall;

Type
TsgcWinHttpWebSocketSend = function(hWebSocket: HINTERNET; eBufferType: Integer; pvBuffer: PVOID; dwBufferLength: DWORD): DWORD; stdcall;
function WinHttpWebSocketSend(hWebSocket: HINTERNET; eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; pvBuffer: PVOID; dwBufferLength: DWORD): DWORD; stdcall;

Type
TsgcWinHttpWebSocketShutdown = function(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;
function WinHttpWebSocketShutdown(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;

{$ENDIF}


implementation

{$IFDEF SGC_WINHTTP}

const
  Winhttpapi = 'winhttp.dll';

{$IFNDEF SGC_WINHTTP_STATIC}
var
  oWinHttpCheckPlatform: TsgcWinHttpCheckPlatform = nil;
  oWinHttpCloseHandle: TsgcWinHttpCloseHandle = nil;
  oWinHttpOpen: TsgcWinHttpOpen = nil;
  oWinHttpConnect: TsgcWinHttpConnect = nil;
  oWinHttpOpenRequest: TsgcWinHttpOpenRequest = nil;
  oWinHttpSetTimeouts: TsgcWinHttpSetTimeouts = nil;
  oWinHttpSetOption: TsgcWinHttpSetOption = nil;
  oWinHttpSetCredentials: TsgcWinHttpSetCredentials = nil;
  oWinHttpAddRequestHeaders: TsgcWinHttpAddRequestHeaders = nil;
  oWinHttpSendRequest: TsgcWinHttpSendRequest = nil;
  oWinHttpReceiveResponse: TsgcWinHttpReceiveResponse = nil;
  oWinHttpQueryHeaders: TsgcWinHttpQueryHeaders = nil;
  oWinHttpSetStatusCallback: TsgcWinHttpSetStatusCallback = nil;
  oWinHttpWebSocketClose: TsgcWinHttpWebSocketClose = nil;
  oWinHttpWebSocketCompleteUpgrade: TsgcWinHttpWebSocketCompleteUpgrade = nil;
  oWinHttpWebSocketQueryCloseStatus: TsgcWinHttpWebSocketQueryCloseStatus = nil;
  oWinHttpWebSocketReceive: TsgcWinHttpWebSocketReceive = nil;
  oWinHttpWebSocketSend: TsgcWinHttpWebSocketSend = nil;
  oWinHttpWebSocketShutdown: TsgcWinHttpWebSocketShutdown = nil;

var
  vInitialized: Boolean = False;
  vModule: HModule = 0;

procedure DoInitialize();
var
  oBuffer: array[0..255] of Char;
begin
  if not vInitialized then
  begin
    vModule := LoadLibrary(PChar(Winhttpapi));
    if (vModule <= HINSTANCE_ERROR) then
    begin
      ZeroMemory(@oBuffer, SizeOf(oBuffer));
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError(), 0, oBuffer, SizeOf(oBuffer), nil);
      raise Exception.Create(oBuffer);
    end;
    vInitialized := True;
  end;
end;

function WinHttpCheckPlatform: BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpCheckPlatform) then
    @oWinHttpCheckPlatform := GetProcAddress(vModule, 'WinHttpCheckPlatform');

  Result := oWinHttpCheckPlatform();
end;


function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpCloseHandle) then
    @oWinHttpCloseHandle := GetProcAddress(vModule, 'WinHttpCloseHandle');

  Result := oWinHttpCloseHandle(hInternet);
end;


function WinHttpOpen(pszAgentW: LPCWSTR; dwAccessType: DWORD; pszProxyW: LPCWSTR; pszProxyBypassW: LPCWSTR;
  dwFlags: DWORD): HINTERNET; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpOpen) then
    @oWinHttpOpen := GetProcAddress(vModule, 'WinHttpOpen');

  Result := oWinHttpOpen(pszAgentW, dwAccessType, pszProxyW, pszProxyBypassW, dwFlags);
end;


function WinHttpConnect(hSession: HINTERNET; pswzServerName: LPCWSTR; nServerPort: INTERNET_PORT;
  dwReserved: DWORD): HINTERNET; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpConnect) then
    @oWinHttpConnect := GetProcAddress(vModule, 'WinHttpConnect');

  Result := oWinHttpConnect(hSession, pswzServerName, nServerPort, dwReserved);
end;


function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb: LPCWSTR; pwszObjectName: LPCWSTR; pwszVersion: LPCWSTR;
  pwszReferrer: LPCWSTR; ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpOpenRequest) then
    @oWinHttpOpenRequest := GetProcAddress(vModule, 'WinHttpOpenRequest');

  Result := oWinHttpOpenRequest(hConnect, pwszVerb, pwszObjectName, pwszVersion, pwszReferrer, ppwszAcceptTypes, dwFlags);
end;


function WinHttpSetTimeouts(hInternet: HINTERNET; nResolveTimeout: Integer; nConnectTimeout: Integer;
  nSendTimeout: Integer; nReceiveTimeout: Integer): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpSetTimeouts) then
    @oWinHttpSetTimeouts := GetProcAddress(vModule, 'WinHttpSetTimeouts');

  Result := oWinHttpSetTimeouts(hInternet, nResolveTimeout, nConnectTimeout, nSendTimeout, nReceiveTimeout);
end;


function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpSetOption) then
    @oWinHttpSetOption := GetProcAddress(vModule, 'WinHttpSetOption');

  Result := oWinHttpSetOption(hInternet, dwOption, lpBuffer, dwBufferLength);
end;


function WinHttpSetCredentials(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD; pwszUserName: LPCWSTR;
  pwszPassword: LPCWSTR; pAuthParams: Pointer): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpSetCredentials) then
    @oWinHttpSetCredentials := GetProcAddress(vModule, 'WinHttpSetCredentials');

  Result := oWinHttpSetCredentials(hRequest, AuthTargets, AuthScheme, pwszUserName, pwszPassword, pAuthParams);
end;


function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: LPCWSTR; dwHeadersLength: DWORD;
  dwModifiers: DWORD): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpAddRequestHeaders) then
    @oWinHttpAddRequestHeaders := GetProcAddress(vModule, 'WinHttpAddRequestHeaders');

  Result := oWinHttpAddRequestHeaders(hRequest, pwszHeaders, dwHeadersLength, dwModifiers);
end;


function WinHttpSendRequest(hRequest: HINTERNET; lpszHeaders: LPCWSTR;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD;
  dwTotalLength: DWORD; dwContext: DWORD_PTR): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpSendRequest) then
    @oWinHttpSendRequest := GetProcAddress(vModule, 'WinHttpSendRequest');

  Result := oWinHttpSendRequest(hRequest, lpszHeaders, dwHeadersLength, lpOptional, dwOptionalLength, dwTotalLength, dwContext);
end;


function WinHttpReceiveResponse(hRequest: HINTERNET; lpReserved: Pointer): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpReceiveResponse) then
    @oWinHttpReceiveResponse := GetProcAddress(vModule, 'WinHttpReceiveResponse');

  Result := oWinHttpReceiveResponse(hRequest, lpReserved);
end;


function WinHttpQueryHeaders(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: LPCWSTR; lpBuffer: LPVOID; lpdwBufferLength: LPDWORD; lpdwIndex: LPDWORD): BOOL; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpQueryHeaders) then
    @oWinHttpQueryHeaders := GetProcAddress(vModule, 'WinHttpQueryHeaders');

  Result := oWinHttpQueryHeaders(hRequest, dwInfoLevel, pwszName, lpBuffer, lpdwBufferLength, lpdwIndex);
end;


function WinHttpSetStatusCallback(hInternet: HINTERNET; lpfnInternetCallback: TWinHttpStatusCallback; dwNotificationFlags: DWORD; dwReserved: DWORD_PTR): TWinHttpStatusCallback; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpSetStatusCallback) then
    @oWinHttpSetStatusCallback := GetProcAddress(vModule, 'WinHttpSetStatusCallback');

  Result := oWinHttpSetStatusCallback(hInternet, lpfnInternetCallback, dwNotificationFlags, dwReserved);
end;


function WinHttpWebSocketClose(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketClose) then
    @oWinHttpWebSocketClose := GetProcAddress(vModule, 'WinHttpWebSocketClose');

  Result := oWinHttpWebSocketClose(hWebSocket, usStatus, pvReason, dwReasonLength);
end;


function WinHttpWebSocketCompleteUpgrade(hRequest: HINTERNET; pContext: DWORD_PTR): HINTERNET; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketCompleteUpgrade) then
    @oWinHttpWebSocketCompleteUpgrade := GetProcAddress(vModule, 'WinHttpWebSocketCompleteUpgrade');

  Result := oWinHttpWebSocketCompleteUpgrade(hRequest, pContext);
end;


function WinHttpWebSocketQueryCloseStatus(hRequest: HINTERNET; pusStatus: PWINHTTP_WEB_SOCKET_CLOSE_STATUS; pvReason: PVOID; dwReasonLength: DWORD; pdwReasonLengthConsumed: PDWORD): DWORD; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketQueryCloseStatus) then
    @oWinHttpWebSocketQueryCloseStatus := GetProcAddress(vModule, 'WinHttpWebSocketQueryCloseStatus');

  Result := oWinHttpWebSocketQueryCloseStatus(hRequest, pusStatus, pvReason, dwReasonLength, pdwReasonLengthConsumed);
end;


function WinHttpWebSocketReceive(hWebSocket: HINTERNET; pvBuffer: PVOID; dwBufferLength: DWORD; pdwBytesRead: Pointer; peBufferType: Pointer): DWORD; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketReceive) then
    @oWinHttpWebSocketReceive := GetProcAddress(vModule, 'WinHttpWebSocketReceive');

  Result := oWinHttpWebSocketReceive(hWebSocket, pvBuffer, dwBufferLength, pdwBytesRead, peBufferType);
end;


function WinHttpWebSocketSend(hWebSocket: HINTERNET; eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; pvBuffer: PVOID; dwBufferLength: DWORD): DWORD; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketSend) then
    @oWinHttpWebSocketSend := GetProcAddress(vModule, 'WinHttpWebSocketSend');

  Result := oWinHttpWebSocketSend(hWebSocket, Ord(eBufferType), pvBuffer, dwBufferLength);
end;


function WinHttpWebSocketShutdown(hWebSocket: HINTERNET; usStatus: USHORT; pvReason: PVOID; dwReasonLength: DWORD): DWORD; stdcall;
begin
  DoInitialize();
  if not Assigned(oWinHttpWebSocketShutdown) then
    @oWinHttpWebSocketShutdown := GetProcAddress(vModule, 'WinHttpWebSocketShutdown');

  Result := oWinHttpWebSocketShutdown(hWebSocket, usStatus, pvReason, dwReasonLength);
end;


{$ENDIF}




{$IFDEF SGC_WINHTTP_STATIC}
function WinHttpCheckPlatform; external Winhttpapi name 'WinHttpCheckPlatform';
function WinHttpCloseHandle; external Winhttpapi name 'WinHttpCloseHandle';
function WinHttpOpen; external Winhttpapi name 'WinHttpOpen';
function WinHttpConnect; external Winhttpapi name 'WinHttpConnect';
function WinHttpOpenRequest; external Winhttpapi name 'WinHttpOpenRequest';
function WinHttpSetTimeouts; external Winhttpapi name 'WinHttpSetTimeouts';
function WinHttpSetOption; external Winhttpapi name 'WinHttpSetOption';
function WinHttpSetCredentials; external Winhttpapi name 'WinHttpSetCredentials';
function WinHttpAddRequestHeaders; external Winhttpapi name 'WinHttpAddRequestHeaders';
function WinHttpSendRequest; external Winhttpapi name 'WinHttpSendRequest';
function WinHttpReceiveResponse; external Winhttpapi name 'WinHttpReceiveResponse';
function WinHttpQueryHeaders; external Winhttpapi name 'WinHttpQueryHeaders';
function WinHttpSetStatusCallback; external Winhttpapi name 'WinHttpSetStatusCallback';

{$WARN SYMBOL_PLATFORM OFF}
function WinHttpWebSocketClose; external Winhttpapi name 'WinHttpWebSocketClose' {$IFDEF D2010}delayed{$ENDIF};
function WinHttpWebSocketCompleteUpgrade; external Winhttpapi name 'WinHttpWebSocketCompleteUpgrade' {$IFDEF D2010}delayed{$ENDIF};
function WinHttpWebSocketQueryCloseStatus; external Winhttpapi name 'WinHttpWebSocketQueryCloseStatus' {$IFDEF D2010}delayed{$ENDIF};
function WinHttpWebSocketReceive; external Winhttpapi name 'WinHttpWebSocketReceive' {$IFDEF D2010}delayed{$ENDIF};
function WinHttpWebSocketSend; external Winhttpapi name 'WinHttpWebSocketSend' {$IFDEF D2010}delayed{$ENDIF};
function WinHttpWebSocketShutdown; external Winhttpapi name 'WinHttpWebSocketShutdown' {$IFDEF D2010}delayed{$ENDIF};
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

{$ENDIF}


end.

