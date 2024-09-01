{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Const;

interface

{$I sgcVer.inc}

Const
  CS_FR_Continuation = $0;
  CS_FR_Text         = $1;
  CS_FR_Binary       = $2;
  CS_FR_Close        = $8;
  CS_FR_Ping         = $9;
  CS_FR_Pong         = $A;

  CS_Frames: Array [0..5] of Integer = (CS_FR_Continuation, CS_FR_Text, CS_FR_Binary,
    CS_FR_Close, CS_FR_Ping, CS_FR_Pong);

  CS_Deflate_Bytes: Array [0..5] of Byte = ($00, $00, $FF, $FF, $03, $00);
  CS_DEFAULT_WINDOW_BITS = 15;
  CS_INFLATE_BUFFER = 4096; //4K
  CS_DEFLATE_BUFFER = 65536; // 64K

  CS_OP_CONTINUATION = 'opContinuation';
  CS_OP_TEXT = 'opText';
  CS_OP_BINARY = 'opBinary';
  CS_OP_CLOSE = 'opClose';
  CS_OP_PING = 'opPing';
  CS_OP_PONG = 'opPong';
  CS_OP_NONE = 'opNone';

Const
  CS_ALPN_H2 = 'h2';

Const
  CS_HTTPAPI_BUFFER_SIZE = 16384;

Const
  CS_DELIMITER = ',';

Const
  CS_HTTP_REQUEST = 'HTTP_REQUEST';

Const
  CS_WS_EXTENSIONS = 'Sec-WebSocket-Extensions: ';

Const
  CS_DEFLATE_FRAME = 'x-webkit-deflate-frame';
  CS_PERMESSAGE_DEFLATE = 'permessage-deflate';

Const
  CS_CLIENT_MAX_WINDOW_BITS = 'client_max_window_bits';
  CS_CLIENT_NO_CONTEXT_TAKE_OVER = 'client_no_context_takeover';
  CS_SERVER_MAX_WINDOW_BITS = 'serv_max_window_bits';
  CS_SERVER_NO_CONTEXT_TAKE_OVER = 'server_no_context_takeover';

Const
  CS_SOCKETIO_URL_API = '/socket.io';
  CS_SOCKETIO_URL_API0 = '/1';
  CS_SOCKETIO_URL_API1_2 = '/?EIO=2';
  CS_SOCKETIO_URL_API3 = '/?EIO=4';
  CS_SOCKETIO_URL_API4 = '/?EIO=4';

{$IFDEF SGC_WRITECONST}Var{$ELSE}Const{$ENDIF}
  CS_REQ_AUTH_SESSION{$IFDEF SGC_WRITECONST}: String{$ENDIF} = '/sgc/req/auth/session/';
  CS_AUTH_SESSION{$IFDEF SGC_WRITECONST}: String{$ENDIF} = '/sgc/auth/session/';
  CS_AUTH_URL{$IFDEF SGC_WRITECONST}: String{$ENDIF} = '/sgc/auth/url/';
Const
  CS_AUTH_BASIC = 'Authorization: Basic';
  CS_AUTH_BEARER = 'Authorization: Bearer';
  CS_AUTH_BEARER_HTTP2 = 'authorization=bearer';
  CS_AUTHORIZATION = 'Authorization: ';
  CS_AUTHORIZATION_HTTP2 = 'authorization=';

Const
  CS_AUTH_NAME_DEFAULT = 'Bearer';

Const
  CS_FLASH_POLICY_REQUEST = '<policy-file-request/>';
  CS_FLASH_WEBSOCKET_FILE = 'WebSocketMain.swf';

Const
  CS_REQ_XHR = '/sgc/xhr/';
  CS_REQ_PROTOCOL = '/sgc/protocol/';

{$IFDEF SGC_WRITECONST}Var{$ELSE}Const{$ENDIF}
  CS_JS_SGCWEBSOCKETS {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'sgcWebSockets.js';
  CS_SGC_JS_SGCWEBSOCKETS {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_SGCWEBSOCKETS';
  CS_HTML_SGCWEBSOCKETS {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'sgcWebSockets.html';
  CS_SGC_HTML_SGCWEBSOCKETS {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_SGCWEBSOCKETS';

  CS_JS_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'esegece.com.js';
  CS_SGC_JS_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_ESEGECE_COM';
  CS_HTML_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'esegece.com.html';
  CS_SGC_HTML_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_ESEGECE_COM';

  CS_JS_DATASET_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'dataset.esegece.com.js';
  CS_SGC_JS_DATASET_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_DATASET_ESEGECE_COM';
  CS_HTML_DATASET_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'dataset.esegece.com.html';
  CS_SGC_HTML_DATASET_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_DATASET_ESEGECE_COM';

  CS_JS_FILES_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'files.esegece.com.js';
  CS_SGC_JS_FILES_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_FILES_ESEGECE_COM';
  CS_HTML_FILES_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'files.esegece.com.html';
  CS_SGC_HTML_FILES_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_FILES_ESEGECE_COM';

  CS_JS_WEBRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'webrtc.esegece.com.js';
  CS_SGC_JS_WEBRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_WEBRTC_ESEGECE_COM';
  CS_HTML_WEBRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'webrtc.esegece.com.html';
  CS_SGC_HTML_WEBRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_WEBRTC_ESEGECE_COM';

  CS_JS_WAMP_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'wamp.esegece.com.js';
  CS_SGC_JS_WAMP_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_WAMP_ESEGECE_COM';
  CS_HTML_WAMP_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'wamp.esegece.com.html';
  CS_SGC_HTML_WAMP_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_WAMP_ESEGECE_COM';

  CS_JS_MQTT_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'mqtt.esegece.com.js';
  CS_SGC_JS_MQTT_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_MQTT_ESEGECE_COM';
  CS_HTML_MQTT_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'mqtt.esegece.com.html';
  CS_SGC_HTML_MQTT_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_MQTT_ESEGECE_COM';

  CS_JS_SGCWEBSOCKETSFLASH {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'sgcWebSocketsFlash.js';
  CS_SGC_JS_SGCWEBSOCKETSFLASH {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_SGCWEBSOCKETSFLASH';
  CS_WEBSOCKETMAIN_SWF {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'WebSocketMain.swf';
  CS_SGC_WEBSOCKETMAIN_SWF {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_WEBSOCKETMAIN_SWF';

  CS_JS_SGCWEBSOCKETSEVENTSOURCE {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'sgcWebSocketsEventSource.js';
  CS_SGC_JS_SGCWEBSOCKETSEVENTSOURCE {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_SGCWEBSOCKETSEVENTSOURCE';

  CS_JS_APPRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'apprtc.esegece.com.js';
  CS_SGC_JS_APPRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_APPRTC_ESEGECE_COM';
  CS_CSS_APPRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'apprtc.esegece.com.css';
  CS_SGC_CSS_APPRTC_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_CSS_APPRTC_ESEGECE_COM';

  CS_JS_PRESENCE_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'presence.esegece.com.js';
  CS_SGC_JS_PRESENCE_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_PRESENCE_ESEGECE_COM';
  CS_HTML_PRESENCE_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'presence.esegece.com.html';
  CS_SGC_HTML_PRESENCE_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_HTML_PRESENCE_ESEGECE_COM';

  CS_JS_RTCMULTICONNECTION_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'rtcmulticonnection.esegece.com.js';
  CS_SGC_JS_RTCMULTICONNECTION_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_JS_RTCMULTICONNECTION_ESEGECE_COM';
  CS_CSS_RTCMULTICONNECTION_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'rtcmulticonnection.esegece.com.css';
  CS_SGC_CSS_RTCMULTICONNECTION_ESEGECE_COM {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'SGC_CSS_RTCMULTICONNECTION_ESEGECE_COM';


Const
  CS_IP = 'ip';
  CS_ACTIVE = 'active';
  CS_HOST = 'host';
  CS_PORT = 'port';
  CS_SSL = 'ssl';

Const
  CS_LB_CLIENT = '/sgc@load_balancer_client';
  CS_LB_CLIENT_CONNECTION = 'sgc@lb@client@connection';
  CS_LB_SERVER_BINDING = 'sgc@lb@server@binding';
  CS_LB_SERVER_READY = 'sgc@lb@server@ready';
  CS_LB_SERVER_DATA = 'sgc@lb@server@data';
  CS_LB_CLIENT_GET_BINDING = 'sgc@lb@client@get@binding';

const
  CS_ENC_B64 = 'sgc@b64:';

{$IFDEF SGC_WRITECONST}Var{$ELSE}Const{$ENDIF}
  CS_PROTOCOL_BROKER {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'broker.esegece.com';
  CS_PROTOCOL_SGC {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'esegece.com';
  CS_PROTOCOL_DATASET {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'dataset.esegece.com';
  CS_PROTOCOL_FILES {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'files.esegece.com';
  CS_PROTOCOL_WEBRTC {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'webrtc.esegece.com';
  CS_PROTOCOL_WAMP {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'WAMP';
  CS_PROTOCOL_WAMP2 {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'wamp.2.json';
  CS_PROTOCOL_MQTT {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'mqtt';
  CS_PROTOCOL_SOCKETIO {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'socketio.esegece.com';
  CS_PROTOCOL_APPRTC {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'apprtc.esegece.com';
  CS_PROTOCOL_PRESENCE {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'presence.esegece.com';
  CS_PROTOCOL_RTCMULTICONNECTION {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'rtcmulticonnection.esegece.com';
  CS_PROTOCOL_AMQP {$IFDEF SGC_WRITECONST}: String{$ENDIF} = 'amqp';

Const
  CS_METHOD = 'method';
  CS_RESULT = 'result';
  CS_PARAMS = 'params';
  CS_MESSAGE = 'message';
  CS_CHANNEL = 'channel';
  CS_PROTOCOL = 'protocol';
  CS_INCLUDE = 'include';
  CS_EXCLUDE = 'exclude';
  CS_ID = 'id';
  CS_GUID = 'guid';
  CS_ERROR = 'error';
  CS_CODE = 'code';
  CS_DATA = 'data';
  CS_QOS = 'qos';
  CS_QOS_LEVEL0 = 'qosLevel0';
  CS_QOS_LEVEL1 = 'qosLevel1';
  CS_QOS_LEVEL2 = 'qosLevel2';
  CS_QUEUE = 'queue';
  CS_QUEUE_LEVEL0 = 'queueLevel0';
  CS_QUEUE_LEVEL1 = 'queueLevel1';
  CS_QUEUE_LEVEL2 = 'queueLevel2';
  CS_JSON_RPC = 'jsonrpc';
  CS_JSONRPC_VERSION = '2.0';
  CS_TEXT_PLAIN = 'text/plain';

Const
  CS_STM_NONE = 'stmNone';
  CS_STM_FIRST = 'stmFirst';
  CS_STM_CONTINUE = 'stmContinue';
  CS_STM_LAST = 'stmLast';
  CS_STM_CONTINUEANDLAST = 'stmContinueAndLast';

Const
  CS_SGC_BROADCAST = 'sgc@broadcast';
  CS_SGC_SUBSCRIBE = 'sgc@subscribe';
  CS_SGC_UNSUBSCRIBE = 'sgc@unsubscribe';
  CS_SGC_UNSUBSCRIBE_ALL = 'sgc@unsubscribe_all';
  CS_SGC_CHANNELS = 'sgc@channels';
  CS_SGC_ACKNOWLEDGMENT = 'sgc@acknowledgment';
  CS_SGC_PROTOCOL = 'sgc@protocol';
  CS_SGC_WEBRTC = 'sgc@webrtc';
  CS_SGC_MESSAGE = 'sgc@message';
  CS_SGC_PUBLISH = 'sgc@publish';
  CS_SGC_EVENT = 'sgc@event';
  CS_SGC_SESSION = 'sgc@session';
  CS_SGC_TRANSACTION = 'sgc@transaction';
  CS_SGC_COMMIT = 'sgc@commit';
  CS_SGC_ROLLBACK = 'sgc@rollback';
  CS_SGC_PUBREC = 'sgc@pubrec';
  CS_SGC_PUBREL = 'sgc@pubrel';

Const
  CS_SGC_FILE = 'sgc@file';
  CS_SGC_FILE_RECEIVED = 'sgc@file_received';
  CS_SGC_FILE_SENT_ERROR = 'sgc@file_sent_error';
  CS_SGC_FILE_RECEIVED_ERROR = 'sgc@file_received_error';
  CS_SGC_FILEREC = 'sgc@filerec';

{ dataset protocol }
Const
  CS_DATASET = 'dataset';
  CS_DATASET_NAME = 'dataset_name';
  CS_DATASET_RECORDCOUNT = 'dataset_recordcount';
  CS_DATASET_RECNO = 'dataset_recno';
  CS_DATASET_ENCODE_BASE64 = 'dataset_encode_base64';
  CS_FIELD = 'field';
  CS_METADATA = 'metadata';
  CS_FIELDNAME = 'fieldname';
  CS_DATATYPE = 'datatype';
  CS_DATASIZE = 'datasize';
  CS_KEYFIELD = 'keyfield';
  CS_SGC_DATASET = 'sgc@dataset';
  CS_SGC_METADATA = 'sgc@metadata';
  CS_SGC_DATASET_SYNCHRONIZE_START = 'sgc@dataset@req@records@start';
  CS_SGC_DATASET_SYNCHRONIZE = 'sgc@dataset@req@records';
  CS_SGC_DATASET_SYNCHRONIZE_END = 'sgc@dataset@req@records@end';
  CS_SGC_DATASET_METADATA = 'sgc@dataset@req@metadata';
  CS_SGC_DATASET_NEW = 'sgc@dataset@new';
  CS_SGC_DATASET_DELETE = 'sgc@dataset@delete';
  CS_SGC_DATASET_UPDATE = 'sgc@dataset@update';
  CS_UPDATE_MODE = 'updatemode';
  CS_UPDATE_WHERE_ALL = 'upWhereAll';
  CS_UPDATE_WHERE_CHANGED = 'upWhereChanged';
  CS_REFRESH_ALL = 'upRefreshAll';
{ dataset protocol }

{ wamp protocol }
Const
  CS_WAMP_VERSION = 1;
Const
  CS_WAMP_WELCOME = 0;
  CS_WAMP_PREFIX = 1;
  CS_WAMP_CALL = 2;
  CS_WAMP_CALLRESULT = 3;
  CS_WAMP_CALLERROR = 4;
  CS_WAMP_SUBSCRIBE = 5;
  CS_WAMP_UNSUBSCRIBE = 6;
  CS_WAMP_PUBLISH = 7;
  CS_WAMP_EVENT = 8;
Const
  CS_WAMP_CALL_PROGRESS_RESULT = 1000;
  CS_WAMP_CALL_CANCEL = 1001;
{ wamp protocol }

{ wamp2 protocol }
Const
  CS_WAMP2_VERSION = 2;
Const
  CS_WAMP2_HELLO = 1;
  CS_WAMP2_WELCOME = 2;
  CS_WAMP2_ABORT = 3;
  CS_WAMP2_CHALLENGE = 4;
  CS_WAMP2_AUTHENTICATE = 5;
  CS_WAMP2_GOODBYE = 6;
  CS_WAMP2_ERROR = 8;
  CS_WAMP2_PUBLISH = 16;
  CS_WAMP2_PUBLISHED = 17;
  CS_WAMP2_SUBSCRIBE = 32;
  CS_WAMP2_SUBSCRIBED = 33;
  CS_WAMP2_UNSUBSCRIBE = 34;
  CS_WAMP2_UNSUBSCRIBED = 35;
  CS_WAMP2_EVENT = 36;
  CS_WAMP2_CALL = 48;
  CS_WAMP2_CANCEL = 49;
  CS_WAMP2_RESULT = 50;
  CS_WAMP2_REGISTER = 64;
  CS_WAMP2_REGISTERED = 65;
  CS_WAMP2_UNREGISTER = 66;
  CS_WAMP2_UNREGISTERED = 67;
  CS_WAMP2_INVOCATION = 68;
  CS_WAMP2_INTERRUPTION = 69;
  CS_WAMP2_YIELD = 70;

Const
  CS_WAMP2_INVOCATION_ERROR = 8001;

Const
  CS_WAMP2_GOODBYE_REPLY = 'wamp.error.goodbye_and_out';
{ wamp2 protocol }

{ presence protocol }
Const
  CS_PRESENCE_SESSION = 'sgc@presence@session';
  CS_PRESENCE_NEW_MEMBER = 'sgc@presence@member@new';
  CS_PRESENCE_REMOVE_MEMBER = 'sgc@presence@member@remove';
  CS_PRESENCE_ACKNOWLEDGMENT = 'sgc@presence@acknowledgment';
  CS_PRESENCE_NEW_CHANNEL_MEMBER = 'sgc@presence@channel@member@new';
  CS_PRESENCE_REMOVE_CHANNEL_MEMBER = 'sgc@presence@channel@member@remove';
  CS_PRESENCE_PUBLISH_MSG = 'sgc@presence@publish@msg';
  CS_PRESENCE_ERROR_MEMBER_CHANNEL = 'sgc@presence@error@member@channel';
  CS_PRESENCE_ERROR_PUBLISH_MSG = 'sgc@presence@error@publish@msg';
  CS_PRESENCE_GET_MEMBERS = 'sgc@presence@get@members';
  CS_PRESENCE_SEND_MEMBERS = 'sgc@presence@send@members';
  CS_PRESENCE_CHANNEL_INVITATION = 'sgc@presence@channel@invitation';
Const
  CS_NAME = 'name';
  CS_INFO = 'info';
  CS_MEMBER = 'member';
  CS_MEMBERS = 'members';
  CS_TEXT = 'text';
  CS_SESSION = 'session';
{ presence protocol }


{ close codes }
Const
  CS_CLOSE_NORMAL = 1000;
  CS_CLOSE_GOING_AWAY = 1001;
  CS_CLOSE_PROTOCOL_ERROR = 1002;
  CS_CLOSE_UNSUPPORTED_DATA = 1003;
  CS_CLOSE_RESERVED = 1004;
  CS_CLOSE_NO_STATUS_RECEIVED = 1005;
  CS_CLOSE_ABNORMAL_CLOSURE = 1006;
  CS_CLOSE_INVALID_PAYLOAD_DATA = 1007;
  CS_CLOSE_POLICY_VIOLATION = 1008;
  CS_CLOSE_MESSAGE_TOO_BIG = 1009;
  CS_CLOSE_MANDATORY_EXTENSION = 1010;
  CS_CLOSE_INTERNAL_SERVER_ERROR = 1011;
  CS_CLOSE_TLS_HANDSHAKE = 1015;
{ close codes }

{ json-rpc 2.0 }
const
  CS_JSONRPC_PARSE_ERROR = -32700;
  CS_JSONRPC_INVALID_REQUEST = -32600;
  CS_JSONRPC_METHOD_NOT_FOUND = -32601;
  CS_JSONRPC_INVALID_PARAMS = -32602;
  CS_JSONRPC_INTERNAL_ERROR = -32603;
  CS_JSONRPC_SERVER_ERROR = -32000;

resourcestring
  S_JSONRPC_PARSE_ERROR = 'Parse error. Invalid JSON was received by the server.';
  S_JSONRPC_INVALID_REQUEST = 'Invalid Request. The JSON sent is not a valid Request object.';
  S_JSONRPC_METHOD_NOT_FOUND = 'Method not found. The method does not exist / is not available.';
  S_JSONRPC_INVALID_PARAMS = 'Invalid params. Invalid method parameter(s)';
  S_JSONRPC_INTERNAL_ERROR = 'Internal JSON-RPC error.';
  S_JSONRPC_SERVER_ERROR = 'Server error.';
{ json-rpc 2.0 }

{ resource strings }
resourcestring
  S_AUTHENTICATION_DENIED = 'Authentication Denied';
  S_CONTROL_FRAMES_UP_125 = 'Control frames are only allowed to have payload up to and including 125 octets';
  S_ERROR_DECODING_MESSAGE = 'Error decoding message.';
  S_ERROR_DECODING_GET = 'Error Decoding Header: GET';
  S_ERROR_DECODING_HOST = 'Error Decoding Header: Host';
  S_ERROR_DECODING_ORIGIN = 'Error Decoding Header: Origin';
  S_ERROR_DECODING_SEC_WEBSOCKET_KEY = 'Error decoding sec-webSocket-key.';
  S_ERROR_DECODING_SWITCHING_PROTOCOLS = 'Error Decoding Header: Switching Protocols';
  S_ERROR_DECODING_UPGRADE = 'Error Decoding Header: Upgrade';
  S_ERROR_DECODING_WEBSOCKET = 'Error Decoding Header: WebSocket';
  S_ERROR_DECODING_WEBSOCKET_KEY = 'Error Decoding Header: WebSocket-Key';
  S_ERROR_DECODING_WEBSOCKET_KEY1 = 'Error Decoding Header: WebSocket-Key1';
  S_ERROR_DECODING_WEBSOCKET_KEY2 = 'Error Decoding Header: WebSocket-Key2';
  S_ERROR_DECODING_WEBSOCKET_VERSION = 'Error Decoding Header: WebSocket Version';
  S_ERROR_DEFLATING_FRAME = 'Error Deflating Frame: %s';
  S_ERROR_INFLATING_FRAME = 'Error Inflating Frame: %s';
  S_ERROR_HANDSHAKE = 'Error Handshake.';
  S_ERROR_HTTP_RESPONSE = 'Error sending HTTP Response.';
  S_HEARTBEAT_TIMEOUT_EXCEEDED = 'HeartBeat Timeout exceeded.';
  S_INVALID_ACCEPT_KEY = 'Handshake Error: Invalid Accept Key';
  S_INVALID_CLOSE_CODE = 'Invalid close code';
  S_INVALID_OPTCODE = 'Invalid Control OptCode';
  S_INVALID_PAYLOAD_LENGTH_CLOSE = 'Invalid Payload length for close code';
  S_INVALID_RSV_NO_EXTENSION = 'Invalid value of RSV while no Extension Negotiated';
  S_INVALID_UTF8_MESSAGE = 'Invalid UTF8 message';
  S_INVALID_WINDOWBITS = 'Invalid Value for WindowBits %d';
  S_MESSAGE_NOT_MASKED = 'Message is not masked';
  S_NORMAL_CLOSE_CODE = 'Normal close code';
  S_OPBINARY_AFTER_CONTINUATION_UNFINISHED = 'OpCode error, received opBinary after Continuation not finished';
  S_OPTCODE_MUST_NOT_FRAGMENTED = 'Control Message must not be fragmented';
  S_OPTEXT_AFTER_CONTINUATION_UNFINISHED = 'OpCode error, received opText after Continuation not finished';
  S_ORIGIN_NOT_ALLOWED = 'Origin %s not allowed';
  S_PAYLOAD_NOT_CORRECT = 'PayLoad is not correct';
  S_PONG_DIFFERENT_PING = 'Received Pong is different from Sent Ping';
  S_PROTOCOL_NOT_SUPPORTED = 'Protocol not supported %s.';
  S_PROTOCOL_UNSUPPORTED = 'Error Protocol: %s not supported.';
  S_FEATURE_UNSUPPORTED = 'Feature not supported.';
  S_UNABLE_GET_SESSION_SOCKETIO = 'Unable to Get Session Id from socket.io server';
  S_UNFRAGMENTED_MESSAGE_AFTER_FIN = 'Unfragmented message after continuation frame with FIN';
  S_UNSUPPORTED_METHOD_SOCKETIO = 'Method not supported by Socket.IO API: %s';
  S_ERROR_RECEIVING_FILE_INVALID_SIZE = 'Error receiving file, expected size of %d and received %d.';
  S_ERROR_ALREADY_SENDING_FILE = 'Already sending a File, wait a try again later.';
  S_ERROR_TIMEOUT_SENDING_FILE = 'File Sending has been cancelled, timeout has been exceeded.';
  S_ERROR_FILE_REJECTED = 'File Rejected.';
  S_ERROR_FILE_CANCELLED = 'File Cancelled.';
  S_ERROR_START_SSL = 'Error starting SSL connection.';
  S_ERROR_READING_BROKER_MESSAGE = 'Error reading Broker Message: %s.';
  S_ERROR_GET_HOST_LOAD_BALANCER = 'Error getting Host from Load Balancer.';
  S_ERROR_SOCKETIO_SID = 'Error reading socket.io session id.';
  S_ERROR_SESSION_HANDLE = 'Error obtaining session handle.';
  S_ERROR_CONNECTING_SERVER = 'Error connecting to server.';
  S_ERROR_OPERATION_CANCELLED = 'Error operation cancelled..';
  S_ERROR_OPEN_REQUEST = 'Error opening request.';
  S_ERROR_RESPONSE_DATA = 'Error obtaining response data.';
  S_ERROR_SYSTEM_CODE = 'System code error: %d.';
  S_INVALID_SERVER_CERTIFICATE = 'Invalid Server Certificate.';
  S_INVALID_CLIENT_CERTIFICATE = 'Invalid Client Certificate.';
  S_INVALID_SERVER_RESPONSE = 'Invalid Server Response.';
  S_ERROR_WRITING_MESSAGE = 'Error writing message %s.';
  S_ERROR_API_NOT_SUPPORTED = 'Error API: %s not supported.';
  S_ERROR_WEBSOCKETS_PROTOCOL_NOT_SUPPORTED = 'Websockets protocol is not supported.';
  S_ERROR_SSL_HASH_NOT_FOUND = 'Error SSL Hash not found.';
  S_ERROR_SIGNALR_TIMEOUT_INVOCATION = 'Invocation has been cancelled, timeout has been exceeded.';
  S_ERROR_H2_INVALID_FRAME_TYPE = 'Invalid Frame Type.';
  S_ERROR_H2_HEADERS_HANDSHAKE = 'Error reading Frame Headers: status <> 200';
  S_ERROR_H2_SETTINGS_ENABLE_CONNECT = 'Error reading Frame Settings: connect is not enabled.';
  S_ERROR_CONNECTION_REJECTED = 'Connection rejected by server.';
{ resource strings }

implementation

end.
