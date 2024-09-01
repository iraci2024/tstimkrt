// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocketsD11.dpk' rev: 35.00 (Windows)

#ifndef Sgcwebsocketsd11HPP
#define Sgcwebsocketsd11HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <sgcJSON.hpp>
#include <sgcIWWebSocket.hpp>
#include <sgcIWWebSocket_Client.hpp>
#include <sgcIWWebSocket_Protocol_Base.hpp>
#include <sgcIWWebSocket_Protocol_Dataset_Client.hpp>
#include <sgcIWWebSocket_Protocol_sgc_Client.hpp>
#include <sgcIWWebSocket_Protocols.hpp>
#include <sgcWebSocket_Protocols.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcWebSocket_CustomClient.hpp>
#include <sgcWebSocket_CustomServer.hpp>
#include <sgcWebSocket_Extensions.hpp>
#include <sgcWebSocket_Extension_Base.hpp>
#include <sgcWebSocket_Extension_DeflateFrame.hpp>
#include <sgcWebSocket_Extension_PerMessage_Deflate.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_LoadBalancer_Client.hpp>
#include <sgcWebSocket_LoadBalancer_Message.hpp>
#include <sgcWebSocket_LoadBalancer_Server.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcWebSocket_Protocol_Broker_Client.hpp>
#include <sgcWebSocket_Protocol_Broker_Message.hpp>
#include <sgcWebSocket_Protocol_Broker_Server.hpp>
#include <sgcWebSocket_Protocol_Dataset_Client.hpp>
#include <sgcWebSocket_Protocol_Dataset_Message.hpp>
#include <sgcWebSocket_Protocol_Dataset_Server.hpp>
#include <sgcWebSocket_Protocol_Files_Client.hpp>
#include <sgcWebSocket_Protocol_Files_Message.hpp>
#include <sgcWebSocket_Protocol_Files_Server.hpp>
#include <sgcWebSocket_Protocol_sgc_Client.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Protocol_sgc_Server.hpp>
#include <sgcWebSocket_Protocol_WebRTC_Message.hpp>
#include <sgcWebSocket_Protocol_WebRTC_Server.hpp>
#include <sgcWebSocket_Protocol_WAMP_Client.hpp>
#include <sgcWebSocket_Protocol_WAMP_Message.hpp>
#include <sgcWebSocket_Protocol_WAMP_Server.hpp>
#include <sgcWebSocket_Protocol_MQTT_Client.hpp>
#include <sgcWebSocket_Protocol_MQTT_Message.hpp>
#include <sgcWebSocket_Protocol_WAMP2_Client.hpp>
#include <sgcWebSocket_Protocol_WAMP2_Message.hpp>
#include <sgcWebSocket_Server.hpp>
#include <sgcWebSocket_Server_Proxy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_Resources.hpp>
#include <sgcWebSocket.hpp>
#include <sgcWebSocket_Client_WinHTTP.hpp>
#include <sgcWebSocket_Classes_WinHTTP.hpp>
#include <sgcWebSocket_WinAPI.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_API_Bitfinex.hpp>
#include <sgcWebSocket_API_Blockchain.hpp>
#include <sgcWebSocket_API_Pusher.hpp>
#include <sgcWebSocket_API_SignalR.hpp>
#include <sgcWebSocket_API_Bittrex.hpp>
#include <sgcWebSocket_API_SocketIO.hpp>
#include <sgcWebSocket_API_Binance.hpp>
#include <sgcWebSocket_API_Bitstamp.hpp>
#include <sgcWebSocket_APIs.hpp>
#include <sgcSTOMP.hpp>
#include <sgcWebSocket_Protocol_STOMP_ActiveMQ_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_Broker_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_Client.hpp>
#include <sgcWebSocket_Protocol_STOMP_Message.hpp>
#include <sgcWebSocket_Protocol_STOMP_RabbitMQ_Client.hpp>
#include <sgcWebSocket_Classes_Sockets.hpp>
#include <sgcWebSocket_Server_Base.hpp>
#include <sgcHTTP.hpp>
#include <sgcTCP_Classes_Indy.hpp>
#include <sgcHTTP_Const.hpp>
#include <sgcWebSocket_API_Huobi.hpp>
#include <sgcHTTP_OAuth_Client.hpp>
#include <sgcHTTP_OAuth2_Client.hpp>
#include <sgcHTTP_Classes.hpp>
#include <sgcHTTP_Helpers.hpp>
#include <sgcWebSocket_API_Cex.hpp>
#include <sgcWebSocket_Protocol_AppRTC_Server.hpp>
#include <sgcWebSocket_Protocol_Presence_Client.hpp>
#include <sgcWebSocket_Protocol_Presence_Message.hpp>
#include <sgcWebSocket_Protocol_Presence_Server.hpp>
#include <sgcWebSocket_API_Bitmex.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_API_SignalRCore.hpp>
#include <sgcWebSocket_API_Kraken.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Const.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcBase_Sync.hpp>
#include <sgcTCP_Client.hpp>
#include <sgcIoT.hpp>
#include <sgcIoT_Classes.hpp>
#include <sgcHTTP_API_Coinbase.hpp>
#include <sgcIoT_Client.hpp>
#include <sgcIoT_MQTT_Client.hpp>
#include <sgcIoT_Amazon_MQTT_Client.hpp>
#include <sgcIoT_Azure_MQTT_Client.hpp>
#include <sgcWebSocket_API_Coinbase.hpp>
#include <sgcWebSocket_API_FXCM.hpp>
#include <sgcSSL_SChannel.hpp>
#include <sgcSSL_SChannel_Indy.hpp>
#include <sgcSSL_WinSSPI.hpp>
#include <sgcHTTP_Client.hpp>
#include <sgcHTTP_Amazon_AWS.hpp>
#include <sgcHTTP_Amazon_SQS.hpp>
#include <sgcHTTP_Amazon_AWS_Signature.hpp>
#include <sgcWebSocket_API_Discord.hpp>
#include <sgcHTTP_API_Binance.hpp>
#include <sgcHTTP_API.hpp>
#include <sgcHTTP_Google_Cloud.hpp>
#include <sgcHTTP_Google_PubSub.hpp>
#include <sgcHTTP_Google_Calendar.hpp>
#include <sgcHTTP_API_Kraken.hpp>
#include <sgcLib_Telegram.hpp>
#include <sgcLib_Telegram_Client.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcUDP_Client.hpp>
#include <sgcP2P.hpp>
#include <sgcUDP_Server.hpp>
#include <sgcUDP_Classes_Indy.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcSocket_Const.hpp>
#include <sgcHTTP_MultipartFormData.hpp>
#include <sgcP2P_STUN_Classes.hpp>
#include <sgcP2P_STUN_Client.hpp>
#include <sgcP2P_STUN_Helpers.hpp>
#include <sgcP2P_STUN_Server.hpp>
#include <sgcP2P_STUN_Types.hpp>
#include <sgcWebSocket_API_FTX.hpp>
#include <sgcHTTP_API_FTX.hpp>
#include <sgcLib_RCON_Client.hpp>
#include <sgcHTTP_API_Cryptohopper.hpp>
#include <sgcLibs.hpp>
#include <sgcHTTP_JWT_HMAC.hpp>
#include <sgcHTTP_JWT_ES.hpp>
#include <sgcHTTP_JWT_Classes.hpp>
#include <sgcHTTP_JWT_Client.hpp>
#include <sgcHTTP_JWT_RSA.hpp>
#include <sgcHTTP_JWT_Helpers.hpp>
#include <sgcHTTP_JWT_Types.hpp>
#include <sgcWebSocket_API_ThreeCommas.hpp>
#include <sgcHTTP_API_ThreeCommas.hpp>
#include <sgcWebSocket_Server_API_RTCMultiConnection.hpp>
#include <sgcWebSocket_Server_API_Base.hpp>
#include <sgcWebSocket_Server_API_SocketIO.hpp>
#include <sgcWebSocket_Server_APIs.hpp>
#include <sgcAMQP.hpp>
#include <sgcAMQP_Classes.hpp>
#include <sgcAMQP_Const.hpp>
#include <sgcAMQP_Helpers.hpp>
#include <sgcAMQP_Client.hpp>
#include <sgcHTTP_API_Bitmex.hpp>
#include <sgcWebSocket_Protocol_AMQP_Client.hpp>
#include <sgcWebSocket_Protocol_AMQP_Message.hpp>
#include <sgcWebSocket_Protocol_AMQP_RabbitMQ_Client.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <IdGlobal.hpp>	// (IndySystem)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <IdFIPS.hpp>	// (IndyProtocols)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <IdWinsock2.hpp>	// (IndySystem)
#include <IdWship6.hpp>	// (IndySystem)
#include <IdIDN.hpp>	// (IndySystem)
#include <IdStackWindows.hpp>	// (IndySystem)
#include <IdStack.hpp>	// (IndySystem)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <IdGlobalProtocols.hpp>	// (IndyProtocols)
#include <IdSSLOpenSSLHeaders.hpp>	// (IndyProtocols)
#include <IdCoderMIME.hpp>	// (IndyProtocols)
#include <IdHMAC.hpp>	// (IndyProtocols)
#include <IdComponent.hpp>	// (IndySystem)
#include <IdIOHandler.hpp>	// (IndyCore)
#include <IdIOHandlerStack.hpp>	// (IndyCore)
#include <IdThread.hpp>	// (IndyCore)
#include <IdSSL.hpp>	// (IndyProtocols)
#include <IdSSLOpenSSL.hpp>	// (IndyProtocols)
#include <IdLogBase.hpp>	// (IndyCore)
#include <IdAuthentication.hpp>	// (IndyProtocols)
#include <IdHTTPHeaderInfo.hpp>	// (IndyProtocols)
#include <IdHeaderCoderBase.hpp>	// (IndyProtocols)
#include <IdHeaderCoderPlain.hpp>	// (IndyProtocols)
#include <IdHeaderCoder2022JP.hpp>	// (IndyProtocols)
#include <IdHeaderCoderIndy.hpp>	// (IndyProtocols)
#include <IdAuthenticationNTLM.hpp>	// (IndyProtocols)
#include <IdAuthenticationSSPI.hpp>	// (IndyProtocols)
#include <IdAuthenticationDigest.hpp>	// (IndyProtocols)
#include <System.NetEncoding.hpp>	// (rtl)
#include <Data.SqlTimSt.hpp>	// (dbrtl)
#include <Data.FmtBcd.hpp>	// (dbrtl)
#include <Data.DB.hpp>	// (dbrtl)
// PRG_EXT: .bpl
// BPI_DIR: ..\dcp\win32
// OBJ_DIR: ..\libD11
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocketsd11
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocketsd11 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKETSD11)
using namespace Sgcwebsocketsd11;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocketsd11HPP
