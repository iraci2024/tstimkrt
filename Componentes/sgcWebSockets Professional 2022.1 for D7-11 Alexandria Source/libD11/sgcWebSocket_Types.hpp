// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Types.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_typesHPP
#define Sgcwebsocket_typesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcWebSocket_Const.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_types
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TOpcode : unsigned char { opContinuation, opText, opBinary, opClose, opPing, opPong, opNone };

typedef System::StaticArray<System::UnicodeString, 7> Sgcwebsocket_types__1;

enum DECLSPEC_DENUM TwsTransports : unsigned char { wsWebSockets, wsEmulation };

enum DECLSPEC_DENUM TwsSpecification : unsigned char { spRFC6455, spHixie76, spRFC8441 };

enum DECLSPEC_DENUM TwsMessageType : unsigned char { mtText, mtBinary };

enum DECLSPEC_DENUM TPayload : unsigned char { pa7bits, pa16bits, pa64bits };

enum DECLSPEC_DENUM TwsServerType : unsigned char { wsTCP, wsHTTP };

enum DECLSPEC_DENUM TwsNotifyEvent : unsigned char { neNoSync, neAsynchronous, neSynchronous };

enum DECLSPEC_DENUM TwsTransport : unsigned char { trpUndefined, trpRFC6455, trpHixie76, trpFlash, trpSSE, trpTCP, trpHTTP, trpRFC8441, trpHTTP2, trpUDP, trpQUIC };

enum DECLSPEC_DENUM TwsApplicationMode : unsigned char { appNone, appServer, appClient };

enum DECLSPEC_DENUM TwsDatasetUpdateMode : unsigned char { upWhereAll, upWhereChanged, upRefreshAll };

typedef System::StaticArray<System::UnicodeString, 3> Sgcwebsocket_types__2;

enum DECLSPEC_DENUM TwsQoS : unsigned char { qosLevel0, qosLevel1, qosLevel2 };

typedef System::StaticArray<System::UnicodeString, 3> Sgcwebsocket_types__3;

enum DECLSPEC_DENUM TwsQueue : unsigned char { queueLevel0, queueLevel1, queueLevel2 };

typedef System::StaticArray<System::UnicodeString, 3> Sgcwebsocket_types__4;

enum DECLSPEC_DENUM TwsSocketIOAPI : unsigned char { ioAPI0, ioAPI1, ioAPI2, ioAPI3, ioAPI4 };

enum DECLSPEC_DENUM TwsFragmentedMessages : unsigned char { frgOnlyBuffer, frgOnlyFragmented, frgAll };

enum DECLSPEC_DENUM TwsStreaming : unsigned char { stmNone, stmFirst, stmContinue, stmLast, stmContinueAndLast };

typedef System::StaticArray<System::UnicodeString, 5> Sgcwebsocket_types__5;

enum DECLSPEC_DENUM TwsLoadBalancing : unsigned char { lbRandom, lbConnections };

enum DECLSPEC_DENUM TwsSSLHandler : unsigned char { sslClient, sslServer, sslSession, sslProxy, sslLoadBalancer };

enum DECLSPEC_DENUM TwsProxy : unsigned char { pxyHTTP, pxySocks4, pxySocks4A, pxySocks5 };

enum DECLSPEC_DENUM TmqttControlPacket : unsigned char { mtcpReserved0, mtcpCONNECT, mtcpCONNACK, mtcpPUBLISH, mtcpPUBACK, mtcpPUBREC, mtcpPUBREL, mtcpPUBCOMP, mtcpSUBSCRIBE, mtcpSUBACK, mtcpUNSUBSCRIBE, mtcpUNSUBACK, mtcpPINGREQ, mtcpPINGRESP, mtcpDISCONNECT, mtcpAUTH };

enum DECLSPEC_DENUM TmqttQoS : unsigned char { mtqsAtMostOnce, mtqsAtLeastOnce, mtqsExactlyOnce, mtqsReserved };

enum DECLSPEC_DENUM TmqttConnReturnCode : unsigned char { mtrcAccepted, mtrcRefusedProtocolVersion, mtrcRefusedIdentifierRejected, mtrcRefusedServerUnavailable, mtrcRefusedUserPassword, mtrcRefusedNotAuthorized };

enum DECLSPEC_DENUM TmqttSUBACK : unsigned char { mqsaUndefined, mqsaSuccess, mqsaFailure };

enum DECLSPEC_DENUM TwsQueueMsgLevels : unsigned char { qmNone, qmLevel0, qmLevel1, qmLevel2 };

enum DECLSPEC_DENUM TwsProtocolMessage : unsigned char { msgText, msgBinary, msgTextBinary };

enum DECLSPEC_DENUM TwsSignalRProtocolVersions : unsigned char { srpt1_2, srpt1_3, srpt1_4, srpt1_5, srpt2_1 };

enum DECLSPEC_DENUM TwsIPVersion : unsigned char { ipV4, ipV6 };

enum DECLSPEC_DENUM TwsAuthentication : unsigned char { authNone, authSession, authURL, authBasic, authOAuth2, authJWT };

enum DECLSPEC_DENUM TwsSignalRCoreProtocols : unsigned char { srcpJSON };

enum DECLSPEC_DENUM TwsSignalRCoreProtocolVersion : unsigned char { srcv1_0 };

enum DECLSPEC_DENUM TwsSignalRCoreMessages : unsigned char { srcmUndefined, srcmInvocation, srcmStreamItem, srcmCompletion, srcmStreamInvocation, srcmCancelInvocation, srcmPing, srcmClose };

enum DECLSPEC_DENUM TwsTLSVersions : unsigned char { tlsUndefined, tls1_0, tls1_1, tls1_2, tls1_3 };

enum DECLSPEC_DENUM TwsTLSIOHandler : unsigned char { iohOpenSSL, iohSChannel };

enum DECLSPEC_DENUM TwsOpenSSLAPI : unsigned char { oslAPI_1_0, oslAPI_1_1, oslAPI_3_0 };

enum DECLSPEC_DENUM TtcpEOFScanBuffer : unsigned char { eofScanNone, eofScanLatestBytes, eofScanAllBytes };

enum DECLSPEC_DENUM TwsIOHandler : unsigned char { iohDefault, iohIOCP };

enum DECLSPEC_DENUM TwsMQTTVersion : unsigned char { mqtt311, mqtt5 };

enum DECLSPEC_DENUM TsgcSChannelCertStorePath : unsigned char { scspStoreCurrentUser, scspStoreLocalMachine };

enum DECLSPEC_DENUM TsgcSChannelCertStoreName : unsigned char { scsnMY, scsnRoot, scsnTrust, scsnCA };

enum DECLSPEC_DENUM TwsPostStreamType : unsigned char { pstMemoryStream, pstFileStream };

enum DECLSPEC_DENUM Th2FragmentedData : unsigned char { h2fdOnlyBuffer, h2fdOnlyFragmented, h2fdAll };

enum DECLSPEC_DENUM TwsOpenSSLLibPath : unsigned char { oslpNone, oslpDefaultFolder, oslpCustomPath };

enum DECLSPEC_DENUM TwsOpenSSLSymLinks : unsigned char { oslsSymLinksDefault, oslsSymLinksLoadFirst, oslsSymLinksLoad, oslsSymLinksDontLoad };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Sgcwebsocket_types__1 TOpCode_String;
extern DELPHI_PACKAGE Sgcwebsocket_types__2 TwsDatasetUpdateMode_String;
extern DELPHI_PACKAGE Sgcwebsocket_types__3 TwsQoS_String;
extern DELPHI_PACKAGE Sgcwebsocket_types__4 TwsQueue_String;
extern DELPHI_PACKAGE Sgcwebsocket_types__5 TwsStreaming_String;
}	/* namespace Sgcwebsocket_types */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_TYPES)
using namespace Sgcwebsocket_types;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_typesHPP
