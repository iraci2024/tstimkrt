// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcP2P_STUN_Types.pas' rev: 35.00 (Windows)

#ifndef Sgcp2p_stun_typesHPP
#define Sgcp2p_stun_typesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcp2p_stun_types
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcSTUNTransport : unsigned char { stunUDP, stunTCP };

enum DECLSPEC_DENUM TsgcStunMessageClass : unsigned char { stmcRequest, stmcIndication, stmcResponseSuccess, stmcResponseError };

enum DECLSPEC_DENUM TsgcStunMessageMethod : unsigned char { stmmUnknown, stmmBinding, stmmReserved, stmmAllocate, stmmRefresh, stmmReserved2, stmmSend, stmmData, stmmCreatePermission, stmmChannelBind };

enum DECLSPEC_DENUM TsgcStunMessageAttribute : unsigned char { stmaMapped_None, stmaMapped_Address, stmaXOR_Mapped_Address, stmaUsername, stmaMesssage_Integrity, stmaFingerprint, stmaError_Code, stmaRealm, stmaNonce, stmaUnknown_Attributes, stmaSoftware, stmaAlternate_Server, stmaChange_Request, stmaResponse_Port, stmaPadding, stmaCache_Timeout, stmaResponse_Origin, stmaOther_Address, stmaSource_Address, stmaChanged_Address, stmaMesssage_Integrity_SHA256, stmaPassword_Algorithm, stmaUserhash, stmaPassword_Algorithms, stmaAlternate_Domain, stmaChannel_Number, stmaLifetime, stmaXOR_Peer_Address, stmaData, stmaXOR_Relayed_Address, stmaRequested_Address_Family, stmaEven_Port, stmaRequested_Transport, stmaDont_Fragment, stmaReservation_Token, 
	stmaAdditional_Address_Family, stmaAddress_Error_Code, stmaICMP, stmaPriority, stmaUse_Candidate, stmaICE_Controlled, stmaICE_Controlling };

typedef System::Set<TsgcStunMessageAttribute, TsgcStunMessageAttribute::stmaMapped_None, TsgcStunMessageAttribute::stmaICE_Controlling> TsgcStunMessageAttributes;

enum DECLSPEC_DENUM TsgcStunFingerprintState : unsigned char { stfsNone, stfsFingerprintValid, stfsFingerprintInvalid };

enum DECLSPEC_DENUM TsgcStunMessageIntegrityState : unsigned char { stisNone, stisMessageIntegrityValid, stisMessageIntegrityInvalid, stisMessageIntegrityInvalidRequest };

enum DECLSPEC_DENUM TsgcStunNonceState : unsigned char { stnsNonceUnknown, stnsNonceValid, stnsNonceStaled };

enum DECLSPEC_DENUM TsgcStunCredentials : unsigned char { stauNone, stauShortTermCredential, stauLongTermCredential };

enum DECLSPEC_DENUM TsgcStunErrorResponseCodes : unsigned char { sercInvalidRequest, sercUnauthorized, sercUnknownAttribute, sercStaleNonce, sercTryAlternate, sercForbidden, sercAllocationMismatch, sercAddressFamilyNotSupported, sercWrongCredentials, sercUnsupportedTransportProtocol, sercPeerAddressFamilyMismatch, sercAllocationQuotaReached, sercInsufficientCapacity, sercICERoleConflict };

enum DECLSPEC_DENUM TsgcStunMessageType : unsigned char { stmtUnknown, stmtClientRequest, stmtClientResponse, stmtServerRequest, stmtServerResponse };

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcp2p_stun_types */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCP2P_STUN_TYPES)
using namespace Sgcp2p_stun_types;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcp2p_stun_typesHPP
