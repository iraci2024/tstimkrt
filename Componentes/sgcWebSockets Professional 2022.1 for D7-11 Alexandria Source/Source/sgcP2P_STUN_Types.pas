{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcP2P_STUN_Types;

interface

{$I sgcVer.inc}
{$IFDEF SGC_STUN}

type
  TsgcSTUNTransport = (stunUDP, stunTCP);

  TsgcStunMessageClass = (stmcRequest, stmcIndication, stmcResponseSuccess,
    stmcResponseError);

  TsgcStunMessageMethod = (stmmUnknown, stmmBinding, stmmReserved, stmmAllocate,
    stmmRefresh, stmmReserved2, stmmSend, stmmData, stmmCreatePermission, stmmChannelBind);

  TsgcStunMessageAttribute = (
    // stun
    stmaMapped_None, stmaMapped_Address, stmaXOR_Mapped_Address, stmaUsername,
    stmaMesssage_Integrity, stmaFingerprint, stmaError_Code, stmaRealm,
    stmaNonce, stmaUnknown_Attributes, stmaSoftware, stmaAlternate_Server,
    stmaChange_Request, stmaResponse_Port, stmaPadding, stmaCache_Timeout,
    stmaResponse_Origin, stmaOther_Address, stmaSource_Address,
    stmaChanged_Address, stmaMesssage_Integrity_SHA256, stmaPassword_Algorithm,
    stmaUserhash, stmaPassword_Algorithms, stmaAlternate_Domain,
    // turn
    stmaChannel_Number, stmaLifetime, stmaXOR_Peer_Address, stmaData,
    stmaXOR_Relayed_Address, stmaRequested_Address_Family, stmaEven_Port,
    stmaRequested_Transport, stmaDont_Fragment, stmaReservation_Token,
    stmaAdditional_Address_Family, stmaAddress_Error_Code, stmaICMP,
    // ice
    stmaPriority, stmaUse_Candidate, stmaICE_Controlled, stmaICE_Controlling);

  TsgcStunMessageAttributes = set of TsgcStunMessageAttribute;

  TsgcStunFingerprintState = (stfsNone, stfsFingerprintValid,
    stfsFingerprintInvalid);
  TsgcStunMessageIntegrityState = (stisNone, stisMessageIntegrityValid,
    stisMessageIntegrityInvalid, stisMessageIntegrityInvalidRequest);

  TsgcStunNonceState = (stnsNonceUnknown, stnsNonceValid, stnsNonceStaled);
  TsgcStunCredentials = (stauNone, stauShortTermCredential,
    stauLongTermCredential);

  TsgcStunErrorResponseCodes = (sercInvalidRequest, sercUnauthorized,
    sercUnknownAttribute, sercStaleNonce, sercTryAlternate, sercForbidden,
    sercAllocationMismatch, sercAddressFamilyNotSupported, sercWrongCredentials,
    sercUnsupportedTransportProtocol, sercPeerAddressFamilyMismatch,
    sercAllocationQuotaReached, sercInsufficientCapacity, sercICERoleConflict);

  TsgcStunMessageType = (stmtUnknown, stmtClientRequest, stmtClientResponse, stmtServerRequest, stmtServerResponse);

{$ENDIF}

implementation

{$IFDEF SGC_STUN}
{$ENDIF}

end.
