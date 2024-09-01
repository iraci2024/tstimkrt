{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Types;

interface

uses
  SysUtils,
  sgcBase_Helpers,
  sgcWebSocket_Const;
{$I sgcVer.inc}

Type
  TOpcode = (opContinuation, opText, opBinary, opClose, opPing, opPong, opNone);

const
  TOpCode_String: array [0 .. 6] of string = (CS_OP_CONTINUATION, CS_OP_TEXT,
    CS_OP_BINARY, CS_OP_CLOSE, CS_OP_PING, CS_OP_PONG, CS_OP_NONE);

Type
  TwsTransports = (wsWebSockets, wsEmulation);
  TwsSpecification = (spRFC6455, spHixie76, spRFC8441);

Type
  TwsMessageType = (mtText, mtBinary);

Type
  TPayload = (pa7bits, pa16bits, pa64bits);

Type
  TwsServerType = (wsTCP, wsHTTP);

Type
  TwsNotifyEvent = (neNoSync, neAsynchronous, neSynchronous);

Type
  TwsTransport = (trpUndefined, trpRFC6455, trpHixie76, trpFlash, trpSSE,
    trpTCP, trpHTTP, trpRFC8441, trpHTTP2, trpUDP, trpQUIC);

Type
  TwsApplicationMode = (appNone, appServer, appClient);

Type
  TwsDatasetUpdateMode = (upWhereAll, upWhereChanged, upRefreshAll);

const
  TwsDatasetUpdateMode_String: array [0 .. 2] of string = (CS_UPDATE_WHERE_ALL,
    CS_UPDATE_WHERE_CHANGED, CS_REFRESH_ALL);

Type
  TwsQoS = (qosLevel0, qosLevel1, qosLevel2);

const
  TwsQoS_String: array [0 .. 2] of string = (CS_QOS_LEVEL0, CS_QOS_LEVEL1,
    CS_QOS_LEVEL2);

Type
  TwsQueue = (queueLevel0, queueLevel1, queueLevel2);

const
  TwsQueue_String: array [0 .. 2] of string = (CS_QUEUE_LEVEL0, CS_QUEUE_LEVEL1,
    CS_QUEUE_LEVEL2);

Type
  TwsSocketIOAPI = (ioAPI0, ioAPI1, ioAPI2, ioAPI3, ioAPI4);

Type
  TwsFragmentedMessages = (frgOnlyBuffer, frgOnlyFragmented, frgAll);

Type
  TwsStreaming = (stmNone, stmFirst, stmContinue, stmLast, stmContinueAndLast);

const
  TwsStreaming_String: array [0 .. 4] of string = (CS_STM_NONE, CS_STM_FIRST,
    CS_STM_CONTINUE, CS_STM_LAST, CS_STM_CONTINUEANDLAST);

Type
  TwsLoadBalancing = (lbRandom, lbConnections);

Type
  TwsSSLHandler = (sslClient, sslServer, sslSession, sslProxy, sslLoadBalancer);

Type
  TwsProxy = (pxyHTTP, pxySocks4, pxySocks4A, pxySocks5);

Type
  TmqttControlPacket = (mtcpReserved0, mtcpCONNECT, mtcpCONNACK, mtcpPUBLISH,
    mtcpPUBACK, mtcpPUBREC, mtcpPUBREL, mtcpPUBCOMP, mtcpSUBSCRIBE, mtcpSUBACK,
    mtcpUNSUBSCRIBE, mtcpUNSUBACK, mtcpPINGREQ, mtcpPINGRESP, mtcpDISCONNECT,
    mtcpAUTH);

Type
  TmqttQoS = (mtqsAtMostOnce, mtqsAtLeastOnce, mtqsExactlyOnce, mtqsReserved);

Type
  TmqttConnReturnCode = (mtrcAccepted, mtrcRefusedProtocolVersion,
    mtrcRefusedIdentifierRejected, mtrcRefusedServerUnavailable,
    mtrcRefusedUserPassword, mtrcRefusedNotAuthorized);

Type
  TmqttSUBACK = (mqsaUndefined, mqsaSuccess, mqsaFailure);

Type
  TwsQueueMsgLevels = (qmNone, qmLevel0, qmLevel1, qmLevel2);

Type
  TwsProtocolMessage = (msgText, msgBinary, msgTextBinary);

Type
  TwsSignalRProtocolVersions = (srpt1_2, srpt1_3, srpt1_4, srpt1_5, srpt2_1);

Type
  TwsIPVersion = (ipV4, ipV6);

Type
  TwsAuthentication = (authNone, authSession, authURL, authBasic, authOAuth2, authJWT);

Type
  TwsSignalRCoreProtocols = (srcpJSON);

Type
  TwsSignalRCoreProtocolVersion = (srcv1_0);

Type
  TwsSignalRCoreMessages = (srcmUndefined, srcmInvocation, srcmStreamItem,
    srcmCompletion, srcmStreamInvocation, srcmCancelInvocation, srcmPing,
    srcmClose);

Type
  TwsTLSVersions = (tlsUndefined, tls1_0, tls1_1, tls1_2, tls1_3);

Type
  TwsTLSIOHandler = (iohOpenSSL, iohSChannel);

Type
  TwsOpenSSLAPI = (oslAPI_1_0, oslAPI_1_1, oslAPI_3_0);

Type
  TtcpEOFScanBuffer = (eofScanNone, eofScanLatestBytes, eofScanAllBytes);

Type
  TwsIOHandler = (iohDefault, iohIOCP);

Type
  TwsMQTTVersion = (mqtt311, mqtt5);

Type
  TsgcSChannelCertStorePath = (scspStoreCurrentUser, scspStoreLocalMachine);
  TsgcSChannelCertStoreName = (scsnMY, scsnRoot, scsnTrust, scsnCA);

Type
  TwsPostStreamType = (pstMemoryStream, pstFileStream);

Type
  Th2FragmentedData = (h2fdOnlyBuffer, h2fdOnlyFragmented, h2fdAll);

Type
  TwsOpenSSLLibPath = (oslpNone, oslpDefaultFolder, oslpCustomPath);

Type
  TwsOpenSSLSymLinks = (oslsSymLinksDefault, oslsSymLinksLoadFirst, oslsSymLinksLoad, oslsSymLinksDontLoad);

implementation

end.
